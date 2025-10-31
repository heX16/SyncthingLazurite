'''
syncthing_manager - Process management for Syncthing with API integration

This is a Python port of the Pascal uSyncthingManager.pas module.
Extends TSyncthingAPI with process management capabilities.

Threading Model:
- Uses asyncio for process management and I/O
- All callbacks are executed in the event loop thread
'''

import asyncio
import os
import sys
import platform
import signal
import time
from typing import Optional, Callable, Any
from enum import Enum
from dataclasses import dataclass, field
import xml.etree.ElementTree as ET

from syncthing_api import TSyncthingAPI


# Constants
PROCESS_START_TIMEOUT_MS = 5000
PROCESS_CHECK_INTERVAL_MS = 100
PROCESS_CHECK_ITERATIONS = PROCESS_START_TIMEOUT_MS // PROCESS_CHECK_INTERVAL_MS
PROCESS_STATE_CHECK_INTERVAL_MS = 1000
API_SHUTDOWN_WAIT_MS = 2000
PROCESS_TERMINATE_WAIT_MS = 1000


# Process states
class TProcessState(Enum):
    '''Process lifecycle states'''
    psUnknown = 'psUnknown'          # Неизвестное состояние
    psStopped = 'psStopped'          # Процесс остановлен
    psStarting = 'psStarting'        # Запуск процесса
    psRunning = 'psRunning'          # Процесс запущен и работает
    psStopping = 'psStopping'        # Остановка процесса
    psError = 'psError'              # Ошибка процесса


@dataclass
class TStartFailureInfo:
    '''Information about process start failure'''
    ErrorMessage: str = ''           # Текст исключения или пусто
    ExecPath: str = ''               # Путь к исполняемому файлу (намеренный)
    IsException: bool = False        # Признак, что ошибка пришла из исключения Execute
    OSLastError: int = 0             # Код последней ошибки ОС (если есть)
    Process: Optional[Any] = None    # Экземпляр процесса


def GetXmlNode(node: Optional[ET.Element], name: str) -> Optional[ET.Element]:
    '''Helper function to find XML nodes (Pascal compatibility)'''
    if node is not None:
        found = node.find(name)
        if found is not None:
            return found
    return None


class TSyncthingManager(TSyncthingAPI):
    '''
    Syncthing manager with process management
    
    Extends TSyncthingAPI with:
    - Process lifecycle management (start/stop)
    - Console output capture
    - Config file parsing (config.xml)
    - Process state machine
    '''
    
    def __init__(self):
        '''Creates the manager object with process management'''
        super().__init__()
        
        # Process management
        self.FProcessSyncthing: Optional[asyncio.subprocess.Process] = None
        self.FProcessState: TProcessState = TProcessState.psUnknown
        
        # Paths
        self.FHomePath: str = ''
        self.FExecPath: str = ''
        
        # Output buffering
        self.FOutputBuffer: bytes = b''
        
        # Timer for process state checking
        self.FTimerCheckProcess: Optional[asyncio.Task] = None
        self._process_timer_cancelled: bool = False
        
        # Output reading task
        self._output_reader_task: Optional[asyncio.Task] = None
        
        # Events (callbacks)
        self.FOnProcessStateChanged: Optional[Callable] = None
        self.FOnConsoleOutput: Optional[Callable] = None
        self.FOnStartFailed: Optional[Callable] = None
        
        # Initialize
        self.InitializeProcesses()
        self.InitializeTimers()
    
    def __del__(self):
        '''Destructor'''
        # Stop process timer
        self._process_timer_cancelled = True
        
        # Note: Async cleanup should be done via explicit Destroy() call
        super().__del__()
    
    async def Destroy(self):
        '''
        Async cleanup method
        Call this explicitly before program exit for clean shutdown
        '''
        # Stop all processes
        await self.StopAllProcesses()
        
        # Stop timers
        self._process_timer_cancelled = True
        if self.FTimerCheckProcess and not self.FTimerCheckProcess.done():
            self.FTimerCheckProcess.cancel()
            try:
                await self.FTimerCheckProcess
            except asyncio.CancelledError:
                pass
        
        # Stop output reader
        if self._output_reader_task and not self._output_reader_task.done():
            self._output_reader_task.cancel()
            try:
                await self._output_reader_task
            except asyncio.CancelledError:
                pass
        
        # Call parent cleanup
        await super().Destroy()
    
    def InitializeProcesses(self):
        '''Initialize process objects (Pascal compatibility)'''
        # In Python, we create subprocess on demand in StartSyncthingProcess
        # This method is kept for API compatibility
        pass
    
    def InitializeTimers(self):
        '''Initialize timers for process state checking'''
        # Timer will be started when process starts
        pass
    
    # Process state management
    
    def ProcessStateChanged(self, NewState: TProcessState):
        '''Handle process state change (virtual method)'''
        old_state = self.FProcessState
        self.FProcessState = NewState
        
        print(f'Process state changed: {old_state.value} -> {NewState.value}')
        
        # Start/stop timer based on state
        if NewState in [TProcessState.psStarting, TProcessState.psRunning]:
            if self.FTimerCheckProcess is None or self.FTimerCheckProcess.done():
                self._process_timer_cancelled = False
                self.FTimerCheckProcess = asyncio.create_task(self._timer_check_process_loop())
        elif NewState in [TProcessState.psStopped, TProcessState.psError]:
            self._process_timer_cancelled = True
            if self.FTimerCheckProcess and not self.FTimerCheckProcess.done():
                self.FTimerCheckProcess.cancel()
            
            # Process remaining output buffer
            if len(self.FOutputBuffer) > 0:
                try:
                    tail_utf = self.FOutputBuffer.decode('utf-8', errors='replace')
                    self.ProcessOutputLine(tail_utf)
                except Exception as e:
                    print(f'Failed to decode trailing console output: {e}')
                self.FOutputBuffer = b''
        
        # Fire callback
        if self.FOnProcessStateChanged:
            self._invoke_callback_sync(self.FOnProcessStateChanged, self, self.FProcessState)
    
    async def _timer_check_process_loop(self):
        '''Timer loop for checking process state'''
        try:
            while not self._process_timer_cancelled:
                await asyncio.sleep(PROCESS_STATE_CHECK_INTERVAL_MS / 1000.0)
                if not self._process_timer_cancelled:
                    self.TimerCheckProcessTimer()
        except asyncio.CancelledError:
            pass
    
    def TimerCheckProcessTimer(self):
        '''Timer handler for process state checking'''
        new_state = TProcessState.psUnknown
        
        is_running = self.IsProcessRunning()
        
        if is_running:
            if self.FProcessState == TProcessState.psStarting:
                new_state = TProcessState.psRunning
            elif self.FProcessState != TProcessState.psRunning:
                new_state = TProcessState.psRunning
        elif self.FProcessState in [TProcessState.psRunning, TProcessState.psStarting]:
            new_state = TProcessState.psStopped
        else:
            new_state = self.FProcessState
        
        if new_state != self.FProcessState:
            self.ProcessStateChanged(new_state)
    
    def ProcessTerminated(self):
        '''Handle process termination'''
        if self.FProcessState == TProcessState.psRunning:
            self.ProcessStateChanged(TProcessState.psStopped)
    
    # Process output handling
    
    async def _read_output_loop(self):
        '''Async loop for reading process output'''
        try:
            if self.FProcessSyncthing and self.FProcessSyncthing.stdout:
                while True:
                    try:
                        # Read line from process stdout
                        line = await self.FProcessSyncthing.stdout.readline()
                        if not line:
                            # EOF reached
                            break
                        
                        # Add to buffer
                        self.FOutputBuffer += line
                        
                        # Process complete lines
                        self.ReadProcessOutput()
                    except Exception as e:
                        print(f'Error reading process output: {e}')
                        break
        except asyncio.CancelledError:
            pass
    
    def ReadProcessOutput(self):
        '''Read and process output from subprocess'''
        if not self.FOutputBuffer:
            return
        
        # Split by line ending
        line_ending = b'\n'
        
        while line_ending in self.FOutputBuffer:
            pos = self.FOutputBuffer.find(line_ending)
            if pos != -1:
                raw_line = self.FOutputBuffer[:pos]
                self.FOutputBuffer = self.FOutputBuffer[pos + len(line_ending):]
                
                # Decode to UTF-8
                try:
                    utf_line = raw_line.decode('utf-8', errors='replace')
                except Exception as e:
                    print(f'Failed to convert console output line to UTF-8; passing raw bytes as UTF-8: {e}')
                    utf_line = str(raw_line)
                
                # Remove trailing \r if present
                utf_line = utf_line.rstrip('\r')
                
                self.ProcessOutputLine(utf_line)
    
    def ProcessReadData(self):
        '''Handle data ready event (Pascal compatibility)'''
        # In async Python version, this is handled by _read_output_loop
        self.ReadProcessOutput()
    
    def ProcessOutputLine(self, Line: str):
        '''Process single line of console output (virtual method)'''
        if self.FOnConsoleOutput:
            self._invoke_callback_sync(self.FOnConsoleOutput, self, Line)
    
    # Config file handling
    
    def GetConfigPath(self) -> str:
        '''Returns path to config.xml'''
        if not self.FHomePath:
            return ''
        return os.path.join(self.FHomePath, 'config.xml')
    
    def ExtractConfigSettings(self):
        '''Extract settings from config.xml (API key, etc.)'''
        if not self.FHomePath:
            return
        
        filename = self.GetConfigPath()
        if not os.path.exists(filename):
            return
        
        try:
            tree = ET.parse(filename)
            root = tree.getroot()
            
            # Navigate to configuration/gui/apikey
            # Using XPath-like find
            gui = root.find('.//gui')
            if gui is not None:
                apikey = gui.find('apikey')
                if apikey is not None and apikey.text:
                    api_key_value = apikey.text.strip()
                    if api_key_value:
                        print('API key loaded from config.xml')
                        self.SetAPIKey(api_key_value)
        except Exception as e:
            print(f'Error parsing config.xml: {e}')
    
    def LoadConfigFromDisk(self):
        '''Load configuration from disk'''
        if self.FHomePath:
            self.ExtractConfigSettings()
    
    # Path setters
    
    def SetHomePath(self, Path: str):
        '''Sets home path for Syncthing'''
        self.FHomePath = Path
    
    def SetExecPath(self, Path: str):
        '''Sets executable path for Syncthing'''
        self.FExecPath = Path
    
    # Process management methods
    
    def StartSyncthingProcess(self) -> bool:
        '''Start Syncthing process'''
        # Note: This is a sync wrapper that schedules async work
        # In a real async app, this should be async
        return asyncio.create_task(self._start_syncthing_process_async()).result()
    
    async def _start_syncthing_process_async(self) -> bool:
        '''Internal async implementation of StartSyncthingProcess'''
        if not self.FExecPath:
            print('Cannot start Syncthing: executable path not set')
            self.ProcessStateChanged(TProcessState.psError)
            return False
        
        if self.FProcessState == TProcessState.psRunning:
            print('Syncthing process already running')
            return True
        
        self.ProcessStateChanged(TProcessState.psStarting)
        
        try:
            # Prepare command line arguments
            args = [
                self.FExecPath,
                '--no-browser',
                f'--home={self.FHomePath}'
            ]
            
            # Start process
            self.FProcessSyncthing = await asyncio.create_subprocess_exec(
                *args,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.STDOUT
            )
            
            # Start output reader
            self._output_reader_task = asyncio.create_task(self._read_output_loop())
            
            # Wait for process to start (with timeout)
            for i in range(PROCESS_CHECK_ITERATIONS):
                if self.IsProcessRunning():
                    break
                await asyncio.sleep(PROCESS_CHECK_INTERVAL_MS / 1000.0)
            
            if self.IsProcessRunning():
                print('Syncthing process started successfully')
                self.ProcessStateChanged(TProcessState.psRunning)
                
                # Connect to API
                self.Connect()
                return True
            else:
                print(f'Failed to start Syncthing process. Exit code: {self.FProcessSyncthing.returncode if self.FProcessSyncthing else "N/A"}')
                self.ProcessStateChanged(TProcessState.psError)
                
                # Fire OnStartFailed
                if self.FOnStartFailed:
                    info = TStartFailureInfo(
                        ErrorMessage='',
                        ExecPath=self.FExecPath,
                        IsException=False,
                        OSLastError=0,
                        Process=self.FProcessSyncthing
                    )
                    self._invoke_callback_sync(self.FOnStartFailed, self, info)
                
                return False
        
        except Exception as e:
            print(f'Exception while starting Syncthing: {e}')
            self.ProcessStateChanged(TProcessState.psError)
            
            # Fire OnStartFailed
            if self.FOnStartFailed:
                info = TStartFailureInfo(
                    ErrorMessage=str(e),
                    ExecPath=self.FExecPath,
                    IsException=True,
                    OSLastError=0,
                    Process=self.FProcessSyncthing
                )
                self._invoke_callback_sync(self.FOnStartFailed, self, info)
            
            return False
    
    def StopSyncthingProcess(self) -> bool:
        '''Stop Syncthing process'''
        # Note: This is a sync wrapper
        return asyncio.create_task(self._stop_syncthing_process_async()).result()
    
    async def _stop_syncthing_process_async(self) -> bool:
        '''Internal async implementation of StopSyncthingProcess'''
        if not self.IsProcessRunning():
            print('Syncthing process not running')
            self.ProcessStateChanged(TProcessState.psStopped)
            return True
        
        self.ProcessStateChanged(TProcessState.psStopping)
        
        try:
            # Try graceful shutdown via API if online
            if self.IsOnline():
                self.API_Get('system/shutdown', None, '')
                await asyncio.sleep(API_SHUTDOWN_WAIT_MS / 1000.0)
            
            # If still running, terminate
            if self.IsProcessRunning():
                if self.FProcessSyncthing:
                    # Send SIGTERM (or equivalent)
                    try:
                        self.FProcessSyncthing.terminate()
                    except ProcessLookupError:
                        pass  # Process already terminated
                
                await asyncio.sleep(PROCESS_TERMINATE_WAIT_MS / 1000.0)
            
            # Check if stopped
            if not self.IsProcessRunning():
                print('Syncthing process stopped successfully')
                self.ProcessStateChanged(TProcessState.psStopped)
                self.Disconnect()  # Disconnect from API
                return True
            else:
                print('Failed to stop Syncthing process')
                self.ProcessStateChanged(TProcessState.psError)
                return False
        
        except Exception as e:
            print(f'Exception while stopping Syncthing: {e}')
            self.ProcessStateChanged(TProcessState.psError)
            return False
    
    def StopAllProcesses(self) -> bool:
        '''Stop all processes'''
        return self.StopSyncthingProcess()
    
    def IsProcessRunning(self) -> bool:
        '''Check if Syncthing process is running'''
        if self.FProcessSyncthing is None:
            return False
        
        # Check if process is still alive
        return self.FProcessSyncthing.returncode is None
    
    # Properties
    
    @property
    def HomePath(self) -> str:
        '''Home path for Syncthing'''
        return self.FHomePath
    
    @HomePath.setter
    def HomePath(self, value: str):
        self.SetHomePath(value)
    
    @property
    def ExecPath(self) -> str:
        '''Executable path for Syncthing'''
        return self.FExecPath
    
    @ExecPath.setter
    def ExecPath(self, value: str):
        self.SetExecPath(value)
    
    @property
    def ProcessState(self) -> TProcessState:
        '''Current process state'''
        return self.FProcessState
    
    # Event properties
    
    @property
    def OnProcessStateChanged(self) -> Optional[Callable]:
        '''Called when process state changes'''
        return self.FOnProcessStateChanged
    
    @OnProcessStateChanged.setter
    def OnProcessStateChanged(self, value: Optional[Callable]):
        self.FOnProcessStateChanged = value
    
    @property
    def OnConsoleOutput(self) -> Optional[Callable]:
        '''Called for each line of console output'''
        return self.FOnConsoleOutput
    
    @OnConsoleOutput.setter
    def OnConsoleOutput(self, value: Optional[Callable]):
        self.FOnConsoleOutput = value
    
    @property
    def OnStartFailed(self) -> Optional[Callable]:
        '''Called when process start fails'''
        return self.FOnStartFailed
    
    @OnStartFailed.setter
    def OnStartFailed(self, value: Optional[Callable]):
        self.FOnStartFailed = value


class TSyncthingManagerWithSupport(TSyncthingManager):
    '''
    Extended version with support process functionality
    Preserved for potential future use
    '''
    
    def __init__(self):
        '''Creates manager with support process'''
        super().__init__()
        self.FProcessSupport: Optional[asyncio.subprocess.Process] = None
        self.InitializeSupportProcess()
    
    def __del__(self):
        '''Destructor'''
        super().__del__()
    
    async def Destroy(self):
        '''Async cleanup'''
        # Stop support process
        if self.FProcessSupport and self.FProcessSupport.returncode is None:
            try:
                self.FProcessSupport.terminate()
                await self.FProcessSupport.wait()
            except:
                pass
        
        await super().Destroy()
    
    def InitializeSupportProcess(self):
        '''Initialize support process (Pascal compatibility)'''
        # In Python, we create subprocess on demand
        pass
    
    async def StartSupportProcess(self) -> bool:
        '''Start support process'''
        if not self.FHomePath:
            print('Cannot start support process: home path not set')
            return False
        
        try:
            args = [
                self.FExecPath,
                f'--home={self.FHomePath}'
            ]
            
            self.FProcessSupport = await asyncio.create_subprocess_exec(
                *args,
                stdout=asyncio.subprocess.DEVNULL,
                stderr=asyncio.subprocess.DEVNULL
            )
            
            result = self.FProcessSupport.returncode is None
            if result:
                print('Support process started successfully')
            else:
                print('Failed to start support process')
            
            return result
        
        except Exception as e:
            print(f'Exception while starting support process: {e}')
            return False
    
    async def StopAllProcesses(self) -> bool:
        '''Stop all processes (main + support)'''
        # Stop main process
        result = await self._stop_syncthing_process_async()
        
        # Stop support process
        if self.FProcessSupport and self.FProcessSupport.returncode is None:
            try:
                self.FProcessSupport.terminate()
                await self.FProcessSupport.wait()
                print('Support process stopped')
            except:
                pass
        
        return result and (self.FProcessSupport is None or self.FProcessSupport.returncode is not None)

