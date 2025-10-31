'''
syncthing_api - Core class for interacting with Syncthing (REST + Event API)

This is a Python port of the Pascal syncthing_api.pas module.
Maintains a unified JSON tree of Syncthing state and manages connection lifecycle via FSM.

Threading Model:
- Uses asyncio for asynchronous operations
- All callbacks are executed in the event loop thread
- Callbacks can be both sync and async functions
- OnBeforeTreeModify/OnAfterTreeModify are NOT for GUI synchronization, but only
  needed if you have additional threads that access the JSON tree concurrently
'''

import asyncio
import json
import time
from typing import Optional, Callable, Any, Dict, List, Set
from enum import Enum
from io import BytesIO

from AsyncHTTP import (
    AsyncHTTP, HttpRequest,
    HTTPErrorCode_SocketConnectTimeout
)


# Finite state machine for the core
class TSyncthingFSM_State(Enum):
    '''Finite state machine states'''
    ssOffline = 'ssOffline'                                      # No active connection; idle/offline state
    ssConnectingInitAndPing = 'ssConnectingInitAndPing'         # Connecting to Syncthing
    ssConnectingPingWait = 'ssConnectingPingWait'               # Connecting to Syncthing
    ssConnectingWaitData = 'ssConnectingWaitData'               # Waiting for initial data
    ssOnline = 'ssOnline'                                       # Connected; long-polling is active
    ssOnlineLongPollingWait = 'ssOnlineLongPollingWait'         # Connected; no error; long-polling is unactive, will connect
    ssOnlineUnstable = 'ssOnlineUnstable'                       # Connected; has error; long-polling has an error and is offline
    ssOnlineUnstableLongPollingWait = 'ssOnlineUnstableLongPollingWait'  # Connected; has error; long-polling is unactive, will connect
    ssOnlinePaused = 'ssOnlinePaused'                           # Connected; long-polling disabled
    ssDisconnecting = 'ssDisconnecting'                         # Graceful disconnect in progress


# Commands for the finite state machine
class TSyncthingFSM_Command(Enum):
    '''FSM commands'''
    ssCmdNone = 'ssCmdNone'
    ssCmdConnect = 'ssCmdConnect'                               # user Cmd
    ssCmdDisconnect = 'ssCmdDisconnect'                         # user Cmd
    ssCmdPause = 'ssCmdPause'                                   # user Cmd
    ssCmdPauseRelease = 'ssCmdPauseRelease'                     # user Cmd
    ssCmdConnectingPingAck = 'ssCmdConnectingPingAck'
    ssCmdConnectingPingFault = 'ssCmdConnectingPingFault'
    ssCmdConnectingTimeout = 'ssCmdConnectingTimeout'
    ssCmdLongPollingForceRestart = 'ssCmdLongPollingForceRestart'
    ssCmdLongPollingConnect = 'ssCmdLongPollingConnect'
    ssCmdLongPollingDataReceived = 'ssCmdLongPollingDataReceived'
    ssCmdLongPollingErrorDisconnected = 'ssCmdLongPollingErrorDisconnected'
    ssCmdLongPollingTimerRestore = 'ssCmdLongPollingTimerRestore'
    ssCmdLongPollingError = 'ssCmdLongPollingError'
    ssCmdQueueEmpty = 'ssCmdQueueEmpty'
    ssCmdDataReceived = 'ssCmdDataReceived'


# Endpoints enum for REST initial sync
# Ref: https://docs.syncthing.net/v2.0.0/dev/rest.html
class TSyncthingEndpointId(Enum):
    '''Syncthing REST API endpoints'''
    epNone = 0
    
    # Config Endpoints ################
    
    # URI: /rest/config (GET/PUT)
    # Returns entire config or replaces it
    # Ref: https://docs.syncthing.net/v2.0.0/rest/config.html
    epConfig = 1
    # URI: /rest/config/restart-required (GET)
    # Returns whether restart is required for current config to take effect
    epConfig_RestartRequired = 2
    # URI: /rest/config/folders (GET/PUT/POST)
    # Returns all folders as array, PUT replaces array, POST adds/replaces single folder
    epConfig_Folders = 3
    # URI: /rest/config/devices (GET/PUT/POST)
    # Returns all devices as array, PUT replaces array, POST adds/replaces single device
    epConfig_Devices = 4
    # URI: /rest/config/folders/*id* (GET/PUT/PATCH/DELETE)
    # Get/replace/patch/delete specific folder by ID
    epConfig_Folders_Subitems = 5
    # URI: /rest/config/devices/*id* (GET/PUT/PATCH/DELETE)
    # Get/replace/patch/delete specific device by ID
    epConfig_Devices_Subitems = 6
    # URI: /rest/config/defaults/folder (GET/PUT/PATCH)
    # Returns template folder config with default values
    epConfig_Defaults_Folder = 7
    # URI: /rest/config/defaults/device (GET/PUT/PATCH)
    # Returns template device config with default values
    epConfig_Defaults_Device = 8
    # URI: /rest/config/defaults/ignores (GET/PUT)
    # Returns default ignore patterns for new folders
    epConfig_Defaults_Ignores = 9
    # URI: /rest/config/options (GET/PUT/PATCH)
    # Global Syncthing options configuration
    epConfig_Options = 10
    # URI: /rest/config/ldap (GET/PUT/PATCH)
    # LDAP authentication configuration
    epConfig_Ldap = 11
    # URI: /rest/config/gui (GET/PUT/PATCH)
    # Web interface configuration
    epConfig_Gui = 12
    
    # System Endpoints ################
    
    # URI: /rest/system/browse (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-browse-get.html
    epSystem_Browse = 13
    # URI: /rest/system/connections (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-connections-get.html
    epSystem_Connections = 14
    epSystem_Debug = 15
    # URI: /rest/system/discovery (GET/POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-discovery-get.html
    epSystem_Discovery = 16
    # URI: /rest/system/error (GET/POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-error-get.html
    epSystem_Error = 17
    # URI: /rest/system/error/clear (POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-error-clear-post.html
    epSystem_Error_Clear = 18
    # URI: /rest/system/log (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-log-get.html
    epSystem_Log = 19
    # URI: /rest/system/log.txt (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-log-get.html#get-rest-system-log-txt
    epSystem_Logtxt = 20
    # URI: /rest/system/loglevels (GET/POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-loglevels-get.html
    epSystem_Loglevels = 21
    # URI: /rest/system/paths (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-paths-get.html
    epSystem_Paths = 22
    # URI: /rest/system/pause (POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-pause-post.html
    epSystem_Pause = 23
    # URI: /rest/system/ping (GET/POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-ping-get.html
    epSystem_Ping = 24
    # URI: /rest/system/reset (POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-reset-post.html
    epSystem_Reset = 25
    # URI: /rest/system/restart (POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-restart-post.html
    epSystem_Restart = 26
    # URI: /rest/system/resume (POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-resume-post.html
    epSystem_Resume = 27
    # URI: /rest/system/shutdown (POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-shutdown-post.html
    epSystem_Shutdown = 28
    # URI: /rest/system/status (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-status-get.html
    epSystem_Status = 29
    # URI: /rest/system/upgrade (GET/POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-upgrade-get.html
    epSystem_Upgrade = 30
    # URI: /rest/system/version (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/system-version-get.html
    epSystem_Version = 31
    
    # Cluster Endpoints ################
    
    # URI: /rest/cluster/pending/devices (GET/DELETE)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/cluster-pending-devices-get.html
    epCluster_Pending_Devices = 32
    # URI: /rest/cluster/pending/folders (GET/DELETE)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/cluster-pending-folders-get.html
    epCluster_Pending_Folders = 33
    
    # Folder Endpoints ################
    
    # URI: /rest/folder/errors (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/folder-errors-get.html
    epFolder_Errors = 34
    # URI: /rest/folder/versions (GET/POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/folder-versions-get.html
    epFolder_Versions = 35
    
    # Database Endpoints ################
    
    # URI: /rest/db/browse (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-browse-get.html
    epDb_Browse = 36
    # URI: /rest/db/completion (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-completion-get.html
    epDb_Completion = 37
    # URI: /rest/db/file (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-file-get.html
    epDb_File = 38
    # URI: /rest/db/ignores (GET/POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-ignores-get.html
    epDb_Ignores = 39
    # URI: /rest/db/localchanged (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-localchanged-get.html
    epDb_LocalChanged = 40
    # URI: /rest/db/need (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-need-get.html
    epDb_Need = 41
    # URI: /rest/db/override (POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-override-post.html
    epDb_Override = 42
    # URI: /rest/db/prio (POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-prio-post.html
    epDb_Prio = 43
    # URI: /rest/db/remoteneed (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-remoteneed-get.html
    epDb_RemoteNeed = 44
    # URI: /rest/db/revert (POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-revert-post.html
    epDb_Revert = 45
    # URI: /rest/db/scan (POST)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-scan-post.html
    epDb_Scan = 46
    # URI: /rest/db/status (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/db-status-get.html
    epDb_Status = 47
    
    # Event Endpoints ################
    
    # URI: /rest/events (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/events-get.html
    epEvents = 48
    # URI: /rest/events/disk (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/events-get.html#get-rest-events-disk
    epEvents_Disk = 49
    
    # Statistics Endpoints ################
    
    # URI: /rest/stats/device (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/stats-device-get.html
    epStats_Device = 50
    # URI: /rest/stats/folder (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/stats-folder-get.html
    epStats_Folder = 51
    
    # Misc Services Endpoints ################
    
    # URI: /rest/svc/deviceid (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/svc-deviceid-get.html
    epSvc_DeviceId = 52
    # URI: /rest/svc/lang (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/svc-lang-get.html
    epSvc_Lang = 53
    # URI: /rest/svc/random/string (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/svc-random-string-get.html
    epSvc_Random_String = 54
    # URI: /rest/svc/report (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/svc-report-get.html
    epSvc_Report = 55
    
    # Debug Endpoints ################
    
    # URI: /rest/debug/...
    # Ref: https://docs.syncthing.net/v2.0.0/rest/debug.html
    epDebug = 56
    
    # Noauth Endpoints ################
    
    # URI: /rest/noauth/health (GET)
    # Ref: https://docs.syncthing.net/v2.0.0/rest/noauth-health-get.html
    epNoauth_Health = 57
    
    epEndpointsCount = 58
    
    epUnknown = 99


# List of the "Base" endpoints.
# This Endpoints is a `JSONTree`.
SyncthingEndpointsBasic: List[TSyncthingEndpointId] = [
    TSyncthingEndpointId.epConfig,
    TSyncthingEndpointId.epSystem_Connections,
    TSyncthingEndpointId.epStats_Device,
    TSyncthingEndpointId.epStats_Folder,
    TSyncthingEndpointId.epSystem_Status,
    TSyncthingEndpointId.epSystem_Version
]

# All "config" endpoints except the `epConfig`
SyncthingEndpointsConfig: List[TSyncthingEndpointId] = [
    TSyncthingEndpointId.epConfig_RestartRequired,
    TSyncthingEndpointId.epConfig_Folders,
    TSyncthingEndpointId.epConfig_Devices,
    TSyncthingEndpointId.epConfig_Folders_Subitems,
    TSyncthingEndpointId.epConfig_Devices_Subitems,
    TSyncthingEndpointId.epConfig_Defaults_Folder,
    TSyncthingEndpointId.epConfig_Defaults_Device,
    TSyncthingEndpointId.epConfig_Defaults_Ignores,
    TSyncthingEndpointId.epConfig_Options,
    TSyncthingEndpointId.epConfig_Ldap,
    TSyncthingEndpointId.epConfig_Gui
]


class TSyncthingAPI:
    '''
    Core class for interacting with Syncthing (REST + Event API) and maintaining JSON tree
    
    THREADING MODEL:
    All callbacks, event handlers, and tree modification operations are executed 
    in the event loop thread via asyncio.
    This means:
    - OnConnected, OnEvent, OnTreeChanged and other callbacks are executed in event loop
    - JSON tree can be safely accessed from event handlers without synchronization
    - OnBeforeTreeModify/OnEndTreeModify are NOT for GUI synchronization, but only
      needed if you have additional threads that access the JSON tree concurrently
    '''
    
    def __init__(self):
        '''Creates the core object without starting any network activity'''
        # FSM state
        self.FState: TSyncthingFSM_State = TSyncthingFSM_State.ssOffline
        self.FStateCommand: TSyncthingFSM_Command = TSyncthingFSM_Command.ssCmdNone
        self.PrevState: TSyncthingFSM_State = TSyncthingFSM_State.ssOffline
        
        # JSON tree
        self.FTreeRoot: dict = self.CreateDefaultRoot()
        
        # Server settings
        self.FAPIKey: str = ''
        self.FHost: str = '127.0.0.1'
        self.FPort: int = 8384
        self.FUseTLS: bool = False
        self.FServerURL: str = ''
        
        # HTTP clients
        self.FHTTP: Optional[AsyncHTTP] = None
        self.FHTTPEvents: Optional[AsyncHTTP] = None
        
        # LongPolling
        self.FEventsLastId: int = 0
        self.FLongPollingIntervalSec: int = 60
        self.FLongPollingWaitGoodBadCount: int = 0
        
        # Timeouts (in milliseconds)
        self.FConnectTimeout: int = 1000
        self.FIOTimeout: int = 1000
        
        # Timers (asyncio tasks)
        self.FTimerLongPollingForceRestart: Optional[asyncio.Task] = None
        self.FTimerConnectingTimeout: Optional[asyncio.Task] = None
        self.FTimerLongPollingErrorRestore: Optional[asyncio.Task] = None
        self.FTimerLongPullingDoConnect: Optional[asyncio.Task] = None
        self._timer_cancelled: bool = False
        
        # Timer intervals (in seconds)
        self._timer_long_polling_force_restart_interval: float = 0.0
        self._timer_connecting_timeout_interval: float = 5.0
        self._timer_long_polling_error_restore_interval: float = 10.0
        self._timer_long_pulling_do_connect_interval: float = 0.5
        
        # Events (callbacks)
        self.FOnBeforeConnect: Optional[Callable] = None
        self.FOnConnected: Optional[Callable] = None
        self.FOnConnectError: Optional[Callable] = None
        self.FOnBeforeDisconnect: Optional[Callable] = None
        self.FOnDisconnectedByUser: Optional[Callable] = None
        self.FOnHardDisconnect: Optional[Callable] = None
        self.FOnLongPollingDrop: Optional[Callable] = None
        self.FOnBeforeLongPollingRestart: Optional[Callable] = None
        self.FOnConnectionUnstable: Optional[Callable] = None
        self.FOnEvent: Optional[Callable] = None
        self.FOnTreeChanged: Optional[Callable] = None
        self.FOnBeforeTreeModify: Optional[Callable] = None
        self.FOnAfterTreeModify: Optional[Callable] = None
        self.FOnStateChanged: Optional[Callable] = None
        
        # Ping state (for synchronous Ping)
        self.FPingInProgress: bool = False
        self.FPingResult: bool = False
        
        # Last connection error code (for emitting OnConnectError from FSM)
        self.FLastConnectErrorCode: int = 0
        
        # Typed JSON pointers (cached references to tree branches)
        self.config: Optional[dict] = None
        self.config_folders: Optional[list] = None
        self.config_devices: Optional[list] = None
        self.config_options: Optional[dict] = None
        self.stats_device: Optional[list] = None
        self.stats_folder: Optional[list] = None
        
        # Update all pointers
        self.UpdateAllJsonPointers()
        
        # Calculate initial timer interval
        self._timer_long_polling_force_restart_interval = self.FLongPollingIntervalSec + 5.0
    
    def __del__(self):
        '''Destructor'''
        # Stop all timers
        self._timer_cancelled = True
        
        # Note: Cleanup of HTTP clients and tasks should be done explicitly
        # via an async Destroy() method if needed
    
    async def Destroy(self):
        '''
        Async cleanup method
        Call this explicitly before program exit for clean shutdown
        '''
        # Stop all timers
        self._timer_cancelled = True
        
        # Cancel all timer tasks
        for timer_task in [
            self.FTimerLongPollingForceRestart,
            self.FTimerConnectingTimeout,
            self.FTimerLongPollingErrorRestore,
            self.FTimerLongPullingDoConnect
        ]:
            if timer_task and not timer_task.done():
                timer_task.cancel()
                try:
                    await timer_task
                except asyncio.CancelledError:
                    pass
        
        # Cleanup HTTP clients
        if self.FHTTP:
            await self.FHTTP.Destroy()
        if self.FHTTPEvents:
            await self.FHTTPEvents.Destroy()
    
    # Static methods
    
    @staticmethod
    def GetEndpointURI(Id: TSyncthingEndpointId) -> str:
        '''Maps endpoint id to REST URI (under /rest/)'''
        endpoint_map = {
            # Config
            TSyncthingEndpointId.epConfig: 'config',
            TSyncthingEndpointId.epConfig_RestartRequired: 'config/restart-required',
            TSyncthingEndpointId.epConfig_Folders: 'config/folders',
            TSyncthingEndpointId.epConfig_Devices: 'config/devices',
            TSyncthingEndpointId.epConfig_Folders_Subitems: 'config/folders/@',
            TSyncthingEndpointId.epConfig_Devices_Subitems: 'config/devices/@',
            TSyncthingEndpointId.epConfig_Defaults_Folder: 'config/defaults/folder',
            TSyncthingEndpointId.epConfig_Defaults_Device: 'config/defaults/device',
            TSyncthingEndpointId.epConfig_Defaults_Ignores: 'config/defaults/ignores',
            TSyncthingEndpointId.epConfig_Options: 'config/options',
            TSyncthingEndpointId.epConfig_Ldap: 'config/ldap',
            TSyncthingEndpointId.epConfig_Gui: 'config/gui',
            
            # System Endpoints
            TSyncthingEndpointId.epSystem_Browse: 'system/browse',
            TSyncthingEndpointId.epSystem_Connections: 'system/connections',
            TSyncthingEndpointId.epSystem_Debug: 'system/debug',
            TSyncthingEndpointId.epSystem_Discovery: 'system/discovery',
            TSyncthingEndpointId.epSystem_Error: 'system/error',
            TSyncthingEndpointId.epSystem_Error_Clear: 'system/error/clear',
            TSyncthingEndpointId.epSystem_Log: 'system/log',
            TSyncthingEndpointId.epSystem_Logtxt: 'system/logtxt',
            TSyncthingEndpointId.epSystem_Loglevels: 'system/loglevels',
            TSyncthingEndpointId.epSystem_Paths: 'system/paths',
            TSyncthingEndpointId.epSystem_Pause: 'system/pause',
            TSyncthingEndpointId.epSystem_Ping: 'system/ping',
            TSyncthingEndpointId.epSystem_Reset: 'system/reset',
            TSyncthingEndpointId.epSystem_Restart: 'system/restart',
            TSyncthingEndpointId.epSystem_Resume: 'system/resume',
            TSyncthingEndpointId.epSystem_Shutdown: 'system/shutdown',
            TSyncthingEndpointId.epSystem_Status: 'system/status',
            TSyncthingEndpointId.epSystem_Upgrade: 'system/upgrade',
            TSyncthingEndpointId.epSystem_Version: 'system/version',
            
            # Cluster
            TSyncthingEndpointId.epCluster_Pending_Devices: 'cluster/pending/devices',
            TSyncthingEndpointId.epCluster_Pending_Folders: 'cluster/pending/folders',
            
            # Folder
            TSyncthingEndpointId.epFolder_Errors: 'folder/errors',
            TSyncthingEndpointId.epFolder_Versions: 'folder/versions',
            
            # Database
            TSyncthingEndpointId.epDb_Browse: 'db/browse',
            TSyncthingEndpointId.epDb_Completion: 'db/completion',
            TSyncthingEndpointId.epDb_File: 'db/file',
            TSyncthingEndpointId.epDb_Ignores: 'db/ignores',
            TSyncthingEndpointId.epDb_LocalChanged: 'db/localchanged',
            TSyncthingEndpointId.epDb_Need: 'db/need',
            TSyncthingEndpointId.epDb_Override: 'db/override',
            TSyncthingEndpointId.epDb_Prio: 'db/prio',
            TSyncthingEndpointId.epDb_RemoteNeed: 'db/remoteneed',
            TSyncthingEndpointId.epDb_Revert: 'db/revert',
            TSyncthingEndpointId.epDb_Scan: 'db/scan',
            TSyncthingEndpointId.epDb_Status: 'db/status',
            
            # Events
            TSyncthingEndpointId.epEvents: 'events',
            TSyncthingEndpointId.epEvents_Disk: 'events/disk',
            
            # Statistics
            TSyncthingEndpointId.epStats_Device: 'stats/device',
            TSyncthingEndpointId.epStats_Folder: 'stats/folder',
            
            # Misc Services
            TSyncthingEndpointId.epSvc_DeviceId: 'svc/deviceid',
            TSyncthingEndpointId.epSvc_Lang: 'svc/lang',
            TSyncthingEndpointId.epSvc_Random_String: 'svc/random/string',
            TSyncthingEndpointId.epSvc_Report: 'svc/report',
            
            # Debug
            TSyncthingEndpointId.epDebug: 'debug',
            
            # Noauth
            TSyncthingEndpointId.epNoauth_Health: 'noauth/health',
        }
        
        return endpoint_map.get(Id, 'invalid_endpoint')
    
    @staticmethod
    def GetEndpointIdByURI(URI: str) -> TSyncthingEndpointId:
        '''Resolves endpoint id by its REST URI; returns epUnknown if not found'''
        # Linear scan over enum values via GetEndpointURI (simple but slow)
        for ep in TSyncthingEndpointId:
            if TSyncthingAPI.GetEndpointURI(ep) == URI:
                return ep
        
        # Additional checks for subitem endpoints with $id placeholder removed
        base = TSyncthingAPI.GetEndpointURI(TSyncthingEndpointId.epConfig_Folders_Subitems).replace('@', '')
        if URI.startswith(base) and len(URI) > len(base):
            return TSyncthingEndpointId.epConfig_Folders_Subitems
        
        base = TSyncthingAPI.GetEndpointURI(TSyncthingEndpointId.epConfig_Devices_Subitems).replace('@', '')
        if URI.startswith(base) and len(URI) > len(base):
            return TSyncthingEndpointId.epConfig_Devices_Subitems
        
        return TSyncthingEndpointId.epUnknown
    
    def GetEndpointJsonTreePath(self, Id: TSyncthingEndpointId) -> str:
        '''Maps endpoint id to "JSON Tree" storage path'''
        # Copy of endpoint URI
        return self.GetEndpointURI(Id)
    
    # JSON Tree methods
    
    def GetDefaultRootStr(self) -> str:
        '''Returns JSON string used to initialize default root'''
        # Initial JSON tree structure (pretty formatted)
        return '''{
  "system" : {
    "connections" : {
      "connections" : {
      },
      "total" : {
        "at" : "",
        "inBytesTotal" : 0,
        "outBytesTotal" : 0
      }
    },
    "status" : {
      "myID" : "",
      "startTime" : "",
      "uptime" : 0
    },
    "version" : {
      "longVersion" : "",
      "version" : ""
    }
  },
  "stats" : {
    "device" : {
    },
    "folder" : {
    }
  },
  "config" : {
    "folders" : [
    ],
    "devices" : [
    ]
  }
}'''
    
    def CreateDefaultRoot(self) -> dict:
        '''Creates default JSON root object used before initial sync'''
        # Create root from default JSON string
        return json.loads(self.GetDefaultRootStr())
    
    def FindAndCreatePathInJsonTree(self, RootObj: dict, JsonPath: str) -> tuple:
        '''
        Finds or creates a path in JSON tree
        Returns (parent, target, target_name)
        '''
        if not JsonPath or JsonPath[0] == '/' or JsonPath[-1] == '/':
            raise Exception('Invalid JSON path format')
        
        parts = JsonPath.split('/')
        
        parent = None
        target = RootObj
        
        for i, part in enumerate(parts):
            # Step into the target
            parent = target
            target_name = part
            
            if isinstance(parent, dict):
                target = parent.get(target_name)
            else:
                raise Exception(f'Parent is not a dict at path {JsonPath}')
            
            # Remove invalid object
            # if object present and is not a last object in path
            if target is not None and (i < len(parts) - 1) and not isinstance(target, dict):
                del parent[target_name]
                target = None
            
            # Add new object to continue path (if needed)
            if target is None:
                parent[target_name] = {}
                target = parent[target_name]
        
        return (parent, target, target_name)
    
    def SetJsonNodeAtPath(self, RootObj: dict, JsonPath: str, NewData: Any):
        '''Replaces a branch in the "JSON Tree" with NewData'''
        parent, target_data, target_name = self.FindAndCreatePathInJsonTree(RootObj, JsonPath)
        
        # Remove the old branch and add the new branch
        if target_name in parent:
            del parent[target_name]
        parent[target_name] = NewData
    
    def JSONTreeSetNewData(self, JsonPath: str, NewData: Any, CallCallback: bool = True):
        '''Public method to set new data in JSON tree'''
        self.SetJsonNodeAtPath(self.FTreeRoot, JsonPath, NewData)
        
        ep_id = self.GetEndpointIdByURI(JsonPath)
        self.UpdateJsonPointer(ep_id)
        if ep_id == TSyncthingEndpointId.epConfig:
            self.UpdateAllJsonPointers()
        
        self.NotifyTreeChanged(JsonPath, ep_id, CallCallback)
    
    def JSONTreeNewDataFromNetwork(self, JsonPath: str, NewData: Any):
        '''Replaces a branch in the "JSON Tree" with NewData from network'''
        self.BeginTreeModify()
        try:
            self.JSONTreeSetNewData(JsonPath, NewData, True)
        finally:
            self.EndTreeModify()
    
    def UpdateJsonPointer(self, EndpointId: TSyncthingEndpointId):
        '''Updates typed JSON pointers (config, stats, etc.) by resolving paths in FTreeRoot'''
        def find_by_path(path: str) -> Any:
            '''Helper to find data by dot-separated path'''
            parts = path.split('.')
            current = self.FTreeRoot
            for part in parts:
                if isinstance(current, dict):
                    current = current.get(part)
                    if current is None:
                        return None
                else:
                    return None
            return current
        
        # Convert endpoint URI to dot-separated path
        uri = self.GetEndpointURI(EndpointId)
        path = uri.replace('/', '.')
        data = find_by_path(path)
        
        # Update corresponding pointer
        if EndpointId == TSyncthingEndpointId.epConfig:
            self.config = data if isinstance(data, dict) else None
        elif EndpointId == TSyncthingEndpointId.epConfig_Folders:
            self.config_folders = data if isinstance(data, list) else None
        elif EndpointId == TSyncthingEndpointId.epConfig_Devices:
            self.config_devices = data if isinstance(data, list) else None
        elif EndpointId == TSyncthingEndpointId.epConfig_Options:
            self.config_options = data if isinstance(data, dict) else None
        elif EndpointId == TSyncthingEndpointId.epStats_Device:
            self.stats_device = data if isinstance(data, list) else None
        elif EndpointId == TSyncthingEndpointId.epStats_Folder:
            self.stats_folder = data if isinstance(data, list) else None
    
    def UpdateAllJsonPointers(self):
        '''Updates all typed JSON pointers using JsonPointerEndpointsList'''
        json_pointer_endpoints_list = [
            TSyncthingEndpointId.epConfig,
            TSyncthingEndpointId.epConfig_Folders,
            TSyncthingEndpointId.epConfig_Devices,
            TSyncthingEndpointId.epConfig_Options,
            TSyncthingEndpointId.epStats_Device,
            TSyncthingEndpointId.epStats_Folder
        ]
        
        for ep in json_pointer_endpoints_list:
            self.UpdateJsonPointer(ep)
    
    def BeginTreeModify(self):
        '''Notifies listeners and prepares to modify the JSON tree'''
        if self.FOnBeforeTreeModify:
            self._invoke_callback_sync(self.FOnBeforeTreeModify, self)
    
    def EndTreeModify(self):
        '''Notifies listeners and finalizes JSON tree modification'''
        if self.FOnAfterTreeModify:
            self._invoke_callback_sync(self.FOnAfterTreeModify, self)
    
    def NotifyTreeChanged(self, Path: str, ep_id: TSyncthingEndpointId = TSyncthingEndpointId.epNone, 
                         CallCallback: bool = True):
        '''Notifies listeners that a tree branch at Path was changed'''
        if self.FOnTreeChanged and CallCallback:
            if ep_id == TSyncthingEndpointId.epNone:
                ep_id = self.GetEndpointIdByURI(Path)
            self._invoke_callback_sync(self.FOnTreeChanged, self, ep_id, Path)
    
    # HTTP client methods
    
    def BuildServerURL(self):
        '''Builds base server URL from host/port and scheme'''
        if self.FUseTLS:
            self.FServerURL = f'https://{self.FHost}:{self.FPort}/'
        else:
            self.FServerURL = f'http://{self.FHost}:{self.FPort}/'
    
    def ConfigureHttpClient(self):
        '''Creates and configures HTTP clients for REST and long-polling'''
        if self.FHTTP is None:
            self.FHTTP = AsyncHTTP()
        self.FHTTP.ConnectTimeout = self.FConnectTimeout
        self.FHTTP.RetryCount = 1
        self.FHTTP.IOTimeout = self.FIOTimeout
        self.FHTTP.KeepConnection = True
        self.FHTTP.OnBeginProcessing = self.HttpAddHeader
        self.FHTTP.OnQueueEmpty = self.OnRestAPIQueueEmpty
        
        if self.FHTTPEvents is None:
            self.FHTTPEvents = AsyncHTTP()
        self.FHTTPEvents.ConnectTimeout = self.FConnectTimeout
        self.FHTTPEvents.RetryCount = 0
        self.FHTTPEvents.IOTimeout = self.FLongPollingIntervalSec * 1000 + self.FIOTimeout
        self.FHTTPEvents.KeepConnection = True
        self.FHTTPEvents.OnBeginProcessing = self.HttpAddHeader
        # TODO: в любом случае будет вызван "request callback". эти обработчики лишние.
        self.FHTTPEvents.OnDisconnected = self.LongPollingDisconnected
        self.FHTTPEvents.OnError = self.LongPollingError
        
        self._timer_long_polling_force_restart_interval = self.FLongPollingIntervalSec + 5.0
    
    def HttpAddHeader(self, Request: HttpRequest, Sender: Any):
        '''Adds X-API-Key header to request'''
        if 'X-API-Key:' not in Request.HeadersRaw:
            if Request.HeadersRaw:
                Request.HeadersRaw = f'X-API-Key: {self.FAPIKey}\n{Request.HeadersRaw}'
            else:
                Request.HeadersRaw = f'X-API-Key: {self.FAPIKey}'
    
    def API_Get(self, Api: str, Callback: Optional[Callable], userString: str):
        '''Internal helper for GET requests'''
        if self.FHTTP is None:
            return
        
        self.FHTTP.Get(
            url=self.FServerURL + 'rest/' + Api,
            callback=Callback,
            headers='',
            operation_name=Api,
            user_object=None,
            user_string=userString,
            clear_duplicates=True
        )
    
    # FSM methods
    
    def SetState(self, Value: TSyncthingFSM_State):
        '''Sets FSM state and triggers OnStateChanged'''
        if self.FState != Value:
            self.FState = Value
            if self.FOnStateChanged:
                self._invoke_callback_sync(self.FOnStateChanged, self, self.FState)
    
    def FSM_Process(self):
        '''State process. (FSM)'''
        def ProcessCmdDisconnect():
            if (self.Command == TSyncthingFSM_Command.ssCmdDisconnect and 
                self.FOnBeforeDisconnect):
                self._invoke_callback_sync(self.FOnBeforeDisconnect, self)
            if self.Command == TSyncthingFSM_Command.ssCmdDisconnect:
                self.SetState(TSyncthingFSM_State.ssDisconnecting)
        
        def OnGotoOffline():
            self.FHTTP.CancelAll()
            # Emit OnConnectError when leaving connecting due to fault/timeout
            if (self.Command == TSyncthingFSM_Command.ssCmdConnectingPingFault or 
                self.Command == TSyncthingFSM_Command.ssCmdConnectingTimeout):
                if self.FOnConnectError:
                    self._invoke_callback_sync(self.FOnConnectError, self, self.FLastConnectErrorCode)
        
        def CheckArea(States: Set[TSyncthingFSM_State]) -> tuple:
            '''Returns (has_event, area_entry)'''
            has_event = False
            area_entry = False
            
            # Check entry into the area
            if self.FState in States and self.PrevState not in States:
                area_entry = True
                has_event = True
            
            # Check the exit from the area
            if self.FState not in States and self.PrevState in States:
                area_entry = False  # exit from area
                has_event = True
            
            return (has_event, area_entry)
        
        print(f'FSM_Process: State={self.FState.value}, Command={self.Command.value}')
        
        for iteration in range(1, 11):
            new_state = (self.PrevState != self.FState)
            
            if new_state:
                print(f'FSM_Process: State={self.FState.value}, Command={self.Command.value} (repeat)')
            
            # "area" processing
            if new_state:
                has_event, area_entry = CheckArea({
                    TSyncthingFSM_State.ssConnectingInitAndPing,
                    TSyncthingFSM_State.ssConnectingPingWait,
                    TSyncthingFSM_State.ssConnectingWaitData
                })
                if has_event:
                    if area_entry:
                        self._start_timer(self.FTimerConnectingTimeout, 
                                        self._timer_connecting_timeout_interval,
                                        self.TimerConnectingTimeoutHandler)
                    else:
                        self._stop_timer(self.FTimerConnectingTimeout)
                
                has_event, area_entry = CheckArea({
                    TSyncthingFSM_State.ssOnlineLongPollingWait,
                    TSyncthingFSM_State.ssOnlineUnstableLongPollingWait
                })
                if has_event:
                    if area_entry:
                        self._start_timer(self.FTimerLongPullingDoConnect,
                                        self._timer_long_pulling_do_connect_interval,
                                        self.TimerLongPullingDoConnectHandler)
                        self.FLongPollingWaitGoodBadCount = 0
                    else:
                        self._stop_timer(self.FTimerLongPullingDoConnect)
                
                has_event, area_entry = CheckArea({
                    TSyncthingFSM_State.ssOnline,
                    TSyncthingFSM_State.ssOnlineLongPollingWait
                })
                if has_event:
                    if area_entry:
                        self._start_timer(self.FTimerLongPollingForceRestart,
                                        self._timer_long_polling_force_restart_interval,
                                        self.TimerLongPollingForceRestartHandler)
                    else:
                        self._stop_timer(self.FTimerLongPollingForceRestart)
                
                has_event, area_entry = CheckArea({
                    TSyncthingFSM_State.ssOnlineUnstable,
                    TSyncthingFSM_State.ssOnlineUnstableLongPollingWait
                })
                if has_event:
                    if area_entry:
                        if self.FOnConnectionUnstable:
                            self._invoke_callback_sync(self.FOnConnectionUnstable, self)
            
            # Automate
            self.PrevState = self.FState
            
            if self.FState == TSyncthingFSM_State.ssConnectingInitAndPing:
                self.BuildServerURL()
                self.ConfigureHttpClient()
                # Start initial ping; HTTP_CheckOnline drives next transitions
                self.API_Get('system/ping', self.HTTP_CheckOnline, '')
                
                self.SetState(TSyncthingFSM_State.ssConnectingPingWait)
            
            elif self.FState == TSyncthingFSM_State.ssConnectingPingWait:
                if (self.Command == TSyncthingFSM_Command.ssCmdConnectingPingFault or
                    self.Command == TSyncthingFSM_Command.ssCmdConnectingTimeout or
                    self.Command == TSyncthingFSM_Command.ssCmdDisconnect):
                    self.SetState(TSyncthingFSM_State.ssOffline)
                    OnGotoOffline()
                
                if self.Command == TSyncthingFSM_Command.ssCmdConnectingPingAck:
                    self.SetState(TSyncthingFSM_State.ssConnectingWaitData)
                    self.LoadAllBasicEndpoints()
            
            elif self.FState == TSyncthingFSM_State.ssConnectingWaitData:
                if self.Command == TSyncthingFSM_Command.ssCmdDataReceived:
                    if not self.IsAnyBasicEndpointQueued():
                        # All data loaded, almost ready to "online status"
                        self.SetState(TSyncthingFSM_State.ssOnlineLongPollingWait)
                        
                        # Formally, we are already connected,
                        # because all data is available.
                        # So, send signal that our status as "online."
                        if self.FOnConnected:
                            self._invoke_callback_sync(self.FOnConnected, self)
                
                if (self.Command == TSyncthingFSM_Command.ssCmdConnectingTimeout or
                    self.Command == TSyncthingFSM_Command.ssCmdDisconnect):
                    self.SetState(TSyncthingFSM_State.ssOffline)
                    OnGotoOffline()
            
            elif self.FState in [TSyncthingFSM_State.ssOnlineLongPollingWait,
                                TSyncthingFSM_State.ssOnlineUnstableLongPollingWait]:
                ProcessCmdDisconnect()
                
                if self.Command == TSyncthingFSM_Command.ssCmdLongPollingConnect:
                    if self.FHTTPEvents.RequestInQueue('polling'):
                        self.FLongPollingWaitGoodBadCount += 1
                    else:
                        self.StartLongPolling()
                
                if self.Command in [TSyncthingFSM_Command.ssCmdLongPollingError,
                                   TSyncthingFSM_Command.ssCmdLongPollingErrorDisconnected]:
                    self.FLongPollingWaitGoodBadCount -= 1
                
                # TODO: create constant for 2 and -10
                if self.FLongPollingWaitGoodBadCount >= 2:
                    self.SetState(TSyncthingFSM_State.ssOnline)
                
                if self.FLongPollingWaitGoodBadCount <= -10:
                    self.SetState(TSyncthingFSM_State.ssOffline)
                
                if self.Command == TSyncthingFSM_Command.ssCmdPause:
                    self.StopLongPolling()
                    self.SetState(TSyncthingFSM_State.ssOnlinePaused)
            
            elif self.FState == TSyncthingFSM_State.ssOnline:
                ProcessCmdDisconnect()
                
                if self.Command in [TSyncthingFSM_Command.ssCmdLongPollingErrorDisconnected,
                                   TSyncthingFSM_Command.ssCmdLongPollingDataReceived]:
                    self.SetState(TSyncthingFSM_State.ssOnlineLongPollingWait)
                
                if self.Command in [TSyncthingFSM_Command.ssCmdLongPollingForceRestart,
                                   TSyncthingFSM_Command.ssCmdLongPollingError]:
                    self.StopLongPolling()
                    self.SetState(TSyncthingFSM_State.ssOnlineLongPollingWait)
                
                if self.Command == TSyncthingFSM_Command.ssCmdPause:
                    self.StopLongPolling()
                    self.SetState(TSyncthingFSM_State.ssOnlinePaused)
            
            elif self.FState == TSyncthingFSM_State.ssOnlineUnstable:
                ProcessCmdDisconnect()
                
                if self.Command == TSyncthingFSM_Command.ssCmdLongPollingTimerRestore:
                    self.SetState(TSyncthingFSM_State.ssOnlineUnstableLongPollingWait)
                
                if self.Command == TSyncthingFSM_Command.ssCmdPause:
                    self.SetState(TSyncthingFSM_State.ssOnlinePaused)
            
            elif self.FState == TSyncthingFSM_State.ssOnlinePaused:
                ProcessCmdDisconnect()
                
                if self.Command == TSyncthingFSM_Command.ssCmdPauseRelease:
                    self.SetState(TSyncthingFSM_State.ssOnlineLongPollingWait)
            
            elif self.FState == TSyncthingFSM_State.ssDisconnecting:
                self.StopLongPolling()
                self.FHTTP.CancelAll()
                self.SetState(TSyncthingFSM_State.ssOffline)
                
                if self.FOnDisconnectedByUser:
                    self._invoke_callback_sync(self.FOnDisconnectedByUser, self)
            
            elif self.FState == TSyncthingFSM_State.ssOffline:
                if self.Command == TSyncthingFSM_Command.ssCmdConnect:
                    if self.FOnBeforeConnect:
                        self._invoke_callback_sync(self.FOnBeforeConnect, self)
                    
                    if self.Command == TSyncthingFSM_Command.ssCmdConnect:
                        self.SetState(TSyncthingFSM_State.ssConnectingInitAndPing)
            
            # Break if state didn't change
            if self.FState == self.PrevState:
                break
        
        self.Command = TSyncthingFSM_Command.ssCmdNone
    
    # Timer methods
    
    def _start_timer(self, timer_field_ref, interval_sec: float, handler: Callable):
        '''Start a timer as asyncio task'''
        # Get the timer field name from the reference
        # This is a bit tricky in Python, but we can use the handler name
        if handler == self.TimerLongPollingForceRestartHandler:
            timer_attr = 'FTimerLongPollingForceRestart'
        elif handler == self.TimerConnectingTimeoutHandler:
            timer_attr = 'FTimerConnectingTimeout'
        elif handler == self.TimerLongPollingErrorRestoreHandler:
            timer_attr = 'FTimerLongPollingErrorRestore'
        elif handler == self.TimerLongPullingDoConnectHandler:
            timer_attr = 'FTimerLongPullingDoConnect'
        else:
            return
        
        # Stop existing timer
        existing_timer = getattr(self, timer_attr)
        if existing_timer and not existing_timer.done():
            existing_timer.cancel()
        
        # Start new timer
        task = asyncio.create_task(self._timer_loop(interval_sec, handler))
        setattr(self, timer_attr, task)
    
    def _stop_timer(self, timer_task: Optional[asyncio.Task]):
        '''Stop a timer task'''
        if timer_task and not timer_task.done():
            timer_task.cancel()
    
    async def _timer_loop(self, interval_sec: float, handler: Callable):
        '''Timer loop that calls handler after interval'''
        try:
            await asyncio.sleep(interval_sec)
            if not self._timer_cancelled:
                await self._invoke_callback_async(handler, self)
        except asyncio.CancelledError:
            pass
    
    def TimerLongPollingForceRestartHandler(self, Sender: Any):
        '''Timer handler for forced long-polling restart'''
        self.Command = TSyncthingFSM_Command.ssCmdLongPollingForceRestart
        
        if self.FOnBeforeLongPollingRestart:
            self._invoke_callback_sync(self.FOnBeforeLongPollingRestart, self)
        
        self.FSM_Process()
    
    def TimerConnectingTimeoutHandler(self, Sender: Any):
        '''Timer handler for connecting timeout'''
        self.Command = TSyncthingFSM_Command.ssCmdConnectingTimeout
        # Mark timeout as error for FSM handler
        self.FLastConnectErrorCode = HTTPErrorCode_SocketConnectTimeout
        self.FSM_Process()
    
    def TimerLongPollingErrorRestoreHandler(self, Sender: Any):
        '''Timer handler for long-polling error restore'''
        self.Command = TSyncthingFSM_Command.ssCmdLongPollingTimerRestore
        self.FSM_Process()
    
    def TimerLongPullingDoConnectHandler(self, Sender: Any):
        '''Timer handler for long-polling do connect'''
        self.Command = TSyncthingFSM_Command.ssCmdLongPollingConnect
        self.FSM_Process()
    
    # Endpoint loading methods
    
    def LoadAllBasicEndpoints(self):
        '''Load all data from REST resources to JSON tree'''
        for ep in SyncthingEndpointsBasic:
            self.LoadEndpoint(ep)
    
    def LoadEndpoint(self, Id: TSyncthingEndpointId):
        '''Loads a single endpoint identified by Id to JSON tree'''
        # Note: pass "JSON target" path via UserString, see `HTTP_RestAPI`
        self.API_Get(
            self.GetEndpointURI(Id),     # REST API Endpoint
            self.GetEndpointCallback(Id),  # callback: usually `HTTP_RestAPI`
            self.GetEndpointURI(Id)        # userString: send JSON-tree target path
        )
    
    def IsAnyBasicEndpointQueued(self) -> bool:
        '''Returns true if any endpoint from SyncthingEndpointsBasic is queued in FHTTP'''
        if self.FHTTP is None:
            return False
        
        for ep in SyncthingEndpointsBasic:
            if self.FHTTP.RequestInQueueCold(self.GetEndpointURI(ep)):
                return True
        
        return False
    
    def GetEndpointCallback(self, Id: TSyncthingEndpointId) -> Callable:
        '''Maps endpoint id to callback handler'''
        # Default to unified handler; specialize here if any endpoint needs custom logic
        return self.HTTP_RestAPI
    
    # Long-polling methods
    
    def StartLongPolling(self) -> bool:
        '''Starts event long-polling listener'''
        if self.FHTTPEvents.RequestInQueue('polling'):
            return False
        
        if self.FTimerLongPollingForceRestart:
            # Restart timer
            self._stop_timer(self.FTimerLongPollingForceRestart)
            self._start_timer(self.FTimerLongPollingForceRestart,
                            self._timer_long_polling_force_restart_interval,
                            self.TimerLongPollingForceRestartHandler)
        
        url = f'{self.FServerURL}rest/events?since={self.FEventsLastId}&limit=10&timeout=60'
        print(f'StartLongPolling: Starting long-polling request to URL={url}')
        
        self.FHTTPEvents.Get(
            url=url,
            callback=self.HTTP_EventAPI,
            headers='',
            operation_name='polling',
            user_object=None,
            user_string=''
        )
        return True
    
    def StopLongPolling(self) -> bool:
        '''Stops event long-polling listener'''
        if self.FHTTPEvents and self.FHTTPEvents.RequestInQueue('polling'):
            print('StopLongPolling')
            self.FHTTPEvents.CancelAll()
            return True
        return False
    
    def LongPollingDisconnected(self, Request: HttpRequest, Sender: Any):
        '''Callback for long-polling disconnected'''
        self.Command = TSyncthingFSM_Command.ssCmdLongPollingErrorDisconnected
        
        if self.FOnLongPollingDrop:
            self._invoke_callback_sync(self.FOnLongPollingDrop, self)
        
        self.FSM_Process()
    
    def LongPollingError(self, Request: HttpRequest, Sender: Any):
        '''Callback for long-polling error'''
        print(f'LongPollingError: HTTP Error Code={Request.Status}, URL={Request.Url}')
        self.Command = TSyncthingFSM_Command.ssCmdLongPollingError
        self.FSM_Process()
    
    def OnRestAPIQueueEmpty(self, Sender: Any):
        '''Callback when REST API queue is empty'''
        if self.FState == TSyncthingFSM_State.ssConnectingWaitData:
            self.Command = TSyncthingFSM_Command.ssCmdQueueEmpty
            self.FSM_Process()
    
    # Event processing methods
    
    def HandleIncommingDataFromEventAPI(self, EventsArray: list):
        '''Parses and processes events array returned by Event API'''
        if not EventsArray:
            return
        
        # TODO: check sequence - forward and backward?
        for item in EventsArray:
            if isinstance(item, dict):
                ev = item
                ev_id = ev.get('id', -1)
                if ev_id >= 0 and ev_id > self.FEventsLastId:
                    self.FEventsLastId = ev_id
                self.ProcessEvent(ev)
    
    def ProcessEvent(self, EventObj: dict):
        '''Dispatches single event to user handler before internal integration'''
        # TODO: TSyncthingAPI.ProcessEvent
        # TODO: Event type constants and event-to-path mapping
        
        if self.FOnEvent:
            self._invoke_callback_sync(self.FOnEvent, self, EventObj)
        
        event_type = EventObj.get('type', '')
        
        if event_type == 'ConfigSaved':
            self.LoadEndpoint(TSyncthingEndpointId.epConfig)
        elif event_type in ['DeviceConnected', 'DeviceDisconnected', 
                           'DevicePaused', 'DeviceResumed']:
            self.LoadEndpoint(TSyncthingEndpointId.epSystem_Connections)
            self.LoadEndpoint(TSyncthingEndpointId.epStats_Device)
        elif event_type in ['FolderSummary', 'FolderErrors', 
                           'FolderPaused', 'FolderResumed', 'FolderCompletion']:
            self.LoadEndpoint(TSyncthingEndpointId.epStats_Folder)
        elif event_type == 'StateChanged':
            self.LoadEndpoint(TSyncthingEndpointId.epSystem_Status)
    
    def IntegrateEvent(self, EventObj: dict):
        '''Integrates single event data into the JSON tree'''
        # Placeholder: integrate fragments from EventObj.get('data') into Root if needed
        assert EventObj is not None
    
    # HTTP callback methods
    
    def ParseJson(self, Request: HttpRequest) -> Optional[Any]:
        '''Parse JSON from HTTP request response'''
        if Request is None:
            return None
        if Request.Status == 499:
            return None
        if Request.Response and Request.Response.getbuffer().nbytes > 0:
            try:
                Request.Response.seek(0)
                data = Request.Response.read()
                return json.loads(data)
            except json.JSONDecodeError:
                return None
        return None
    
    def HTTP_CheckOnline(self, Request: HttpRequest):
        '''HTTP callback for checking if server is online'''
        if Request and Request.Status == 200 and Request.Succeeded:
            # Is online
            self.Command = TSyncthingFSM_Command.ssCmdConnectingPingAck
            self.FSM_Process()
        else:
            # Store error code, FSM will emit OnConnectError
            if Request is None:
                self.FLastConnectErrorCode = 0
            else:
                self.FLastConnectErrorCode = Request.Status
            self.Command = TSyncthingFSM_Command.ssCmdConnectingPingFault
            self.FSM_Process()
    
    def HTTP_Ping(self, Request: HttpRequest):
        '''HTTP callback for synchronous Ping'''
        # Set result based on HTTP status and success flag
        self.FPingResult = (Request is not None and 
                           Request.Status == 200 and 
                           Request.Succeeded)
        self.FPingInProgress = False
    
    def HTTP_RestAPI(self, Request: HttpRequest):
        '''Universal callback for REST API requests'''
        if Request:
            if not Request.UserString:
                return
            
            parsed_json = self.ParseJson(Request)
            if parsed_json is not None:
                self.JSONTreeNewDataFromNetwork(Request.UserString, parsed_json)
                
                self.Command = TSyncthingFSM_Command.ssCmdDataReceived
                self.FSM_Process()
    
    def HTTP_EventAPI(self, Request: HttpRequest):
        '''HTTP callback for Event API (long-polling)'''
        if Request.Succeeded:
            print(f'HTTP_EventAPI: Received event data, StatusCode={Request.Status}')
            parsed_json = self.ParseJson(Request)
            if parsed_json is not None and isinstance(parsed_json, list):
                print(f'HTTP_EventAPI: Parsed {len(parsed_json)} events')
                self.HandleIncommingDataFromEventAPI(parsed_json)
                self.Command = TSyncthingFSM_Command.ssCmdLongPollingDataReceived
                self.FSM_Process()
    
    # Public API methods
    
    def Connect(self):
        '''Connects to a running Syncthing instance and performs initial sync'''
        self.Command = TSyncthingFSM_Command.ssCmdConnect
        self.FSM_Process()
    
    def Disconnect(self):
        '''Gracefully disconnects and stops long-polling'''
        self.Command = TSyncthingFSM_Command.ssCmdDisconnect
        self.FSM_Process()
    
    def Pause(self):
        '''Pause long-polling'''
        if self.IsOnline():
            self.Command = TSyncthingFSM_Command.ssCmdPause
            self.FSM_Process()
    
    def PauseRelease(self):
        '''Release pause and resume long-polling'''
        if self.IsOnline():
            self.Command = TSyncthingFSM_Command.ssCmdPauseRelease
            self.FSM_Process()
    
    def Ping(self) -> bool:
        '''
        Ping. Allows checking if Syncthing is reachable without establishing a full connection.
        Synchronous operation - the call blocks the current thread until completion or timeout.
        '''
        # Ensure HTTP client and base URL are configured
        self.BuildServerURL()
        self.ConfigureHttpClient()
        
        # Set initial state for ping
        self.FPingInProgress = True
        self.FPingResult = False
        
        # Fire async request
        self.API_Get('system/ping', self.HTTP_Ping, '')
        
        # Block current thread until ping completes or times out
        # Use IOTimeout as maximum wait budget; fall back to ConnectTimeout if zero
        # Minimal sleep to reduce CPU load while allowing callbacks to execute
        timeout_ms = self.FIOTimeout + self.FConnectTimeout
        elapsed = 0
        sleep_ms = 10
        
        while self.FPingInProgress and elapsed < timeout_ms:
            time.sleep(sleep_ms / 1000.0)
            elapsed += sleep_ms
        
        return self.FPingResult
    
    def SetEndpoint(self, Host: str, Port: int, UseTLS: bool):
        '''Sets endpoint (host, port, TLS flag) and rebuilds base URL'''
        self.FHost = Host
        self.FPort = Port
        self.FUseTLS = UseTLS
        self.BuildServerURL()
    
    def SetAPIKey(self, Key: str):
        '''Sets X-API-Key for authenticated requests'''
        self.FAPIKey = Key
    
    def SetLongPollingRestartInterval(self, Seconds: int):
        '''Sets periodic restart interval for long-polling (0 to disable)'''
        self.FLongPollingIntervalSec = Seconds
        self.ConfigureHttpClient()
    
    def IsOnline(self) -> bool:
        '''Returns true when FSM is online'''
        return self.FState in [
            TSyncthingFSM_State.ssOnline,
            TSyncthingFSM_State.ssOnlinePaused,
            TSyncthingFSM_State.ssOnlineUnstable
        ]
    
    # Properties
    
    @property
    def State(self) -> TSyncthingFSM_State:
        '''Current finite state machine state'''
        return self.FState
    
    @property
    def Command(self) -> TSyncthingFSM_Command:
        '''Current finite state machine command'''
        return self.FStateCommand
    
    @Command.setter
    def Command(self, value: TSyncthingFSM_Command):
        self.FStateCommand = value
    
    @property
    def TreeRoot(self) -> dict:
        '''In-memory JSON root with all synchronized data'''
        return self.FTreeRoot
    
    @property
    def ConnectTimeout(self) -> int:
        '''Connection timeout in milliseconds'''
        return self.FConnectTimeout
    
    @ConnectTimeout.setter
    def ConnectTimeout(self, value: int):
        self.FConnectTimeout = value
        if self.FHTTP:
            self.FHTTP.ConnectTimeout = value
        if self.FHTTPEvents:
            self.FHTTPEvents.ConnectTimeout = value
    
    @property
    def IOTimeout(self) -> int:
        '''I/O timeout in milliseconds'''
        return self.FIOTimeout
    
    @IOTimeout.setter
    def IOTimeout(self, value: int):
        self.FIOTimeout = value
        if self.FHTTP:
            self.FHTTP.IOTimeout = value
        if self.FHTTPEvents:
            self.FHTTPEvents.IOTimeout = self.FLongPollingIntervalSec * 1000 + value
    
    @property
    def ConnectingTimeout(self) -> int:
        '''Connecting timeout in milliseconds'''
        return int(self._timer_connecting_timeout_interval * 1000)
    
    @ConnectingTimeout.setter
    def ConnectingTimeout(self, value: int):
        self._timer_connecting_timeout_interval = value / 1000.0
    
    # Callback properties
    
    @property
    def OnBeforeConnect(self) -> Optional[Callable]:
        '''Called before starting connection procedure'''
        return self.FOnBeforeConnect
    
    @OnBeforeConnect.setter
    def OnBeforeConnect(self, value: Optional[Callable]):
        self.FOnBeforeConnect = value
    
    @property
    def OnConnected(self) -> Optional[Callable]:
        '''Called after successful connection and initial sync'''
        return self.FOnConnected
    
    @OnConnected.setter
    def OnConnected(self, value: Optional[Callable]):
        self.FOnConnected = value
    
    @property
    def OnConnectError(self) -> Optional[Callable]:
        '''Called if connection or initial sync fails'''
        return self.FOnConnectError
    
    @OnConnectError.setter
    def OnConnectError(self, value: Optional[Callable]):
        self.FOnConnectError = value
    
    @property
    def OnBeforeDisconnect(self) -> Optional[Callable]:
        '''Called before starting disconnect procedure'''
        return self.FOnBeforeDisconnect
    
    @OnBeforeDisconnect.setter
    def OnBeforeDisconnect(self, value: Optional[Callable]):
        self.FOnBeforeDisconnect = value
    
    @property
    def OnDisconnectedByUser(self) -> Optional[Callable]:
        '''Called after user-requested disconnect finishes'''
        return self.FOnDisconnectedByUser
    
    @OnDisconnectedByUser.setter
    def OnDisconnectedByUser(self, value: Optional[Callable]):
        self.FOnDisconnectedByUser = value
    
    @property
    def OnHardDisconnect(self) -> Optional[Callable]:
        '''Called when connection is lost and recovery failed'''
        return self.FOnHardDisconnect
    
    @OnHardDisconnect.setter
    def OnHardDisconnect(self, value: Optional[Callable]):
        self.FOnHardDisconnect = value
    
    @property
    def OnLongPollingDrop(self) -> Optional[Callable]:
        '''Called when long-polling drops; return action to take'''
        return self.FOnLongPollingDrop
    
    @OnLongPollingDrop.setter
    def OnLongPollingDrop(self, value: Optional[Callable]):
        self.FOnLongPollingDrop = value
    
    @property
    def OnBeforeLongPollingRestart(self) -> Optional[Callable]:
        '''Called before periodic long-polling restart'''
        return self.FOnBeforeLongPollingRestart
    
    @OnBeforeLongPollingRestart.setter
    def OnBeforeLongPollingRestart(self, value: Optional[Callable]):
        self.FOnBeforeLongPollingRestart = value
    
    @property
    def OnConnectionUnstable(self) -> Optional[Callable]:
        '''Called when entering unstable online state'''
        return self.FOnConnectionUnstable
    
    @OnConnectionUnstable.setter
    def OnConnectionUnstable(self, value: Optional[Callable]):
        self.FOnConnectionUnstable = value
    
    @property
    def OnEvent(self) -> Optional[Callable]:
        '''Called on every received raw event (before integration)'''
        return self.FOnEvent
    
    @OnEvent.setter
    def OnEvent(self, value: Optional[Callable]):
        self.FOnEvent = value
    
    @property
    def OnTreeChanged(self) -> Optional[Callable]:
        '''Called after integration when a JSON tree path was updated'''
        return self.FOnTreeChanged
    
    @OnTreeChanged.setter
    def OnTreeChanged(self, value: Optional[Callable]):
        self.FOnTreeChanged = value
    
    @property
    def OnBeforeTreeModify(self) -> Optional[Callable]:
        '''
        Called before modifying the JSON tree.
        NOT needed for GUI synchronization - only use if you have additional threads accessing the tree.
        '''
        return self.FOnBeforeTreeModify
    
    @OnBeforeTreeModify.setter
    def OnBeforeTreeModify(self, value: Optional[Callable]):
        self.FOnBeforeTreeModify = value
    
    @property
    def OnAfterTreeModify(self) -> Optional[Callable]:
        '''
        Called after modifying the JSON tree.
        NOT needed for GUI synchronization - only use if you have additional threads accessing the tree.
        '''
        return self.FOnAfterTreeModify
    
    @OnAfterTreeModify.setter
    def OnAfterTreeModify(self, value: Optional[Callable]):
        self.FOnAfterTreeModify = value
    
    @property
    def OnStateChanged(self) -> Optional[Callable]:
        '''Called when FSM state changes'''
        return self.FOnStateChanged
    
    @OnStateChanged.setter
    def OnStateChanged(self, value: Optional[Callable]):
        self.FOnStateChanged = value
    
    # Helper methods for callback invocation
    
    def _invoke_callback_sync(self, callback: Callable, *args):
        '''Invoke callback synchronously (for both sync and async functions)'''
        if asyncio.iscoroutinefunction(callback):
            # Schedule async callback
            asyncio.create_task(callback(*args))
        else:
            # Call sync callback directly
            callback(*args)
    
    async def _invoke_callback_async(self, callback: Callable, *args):
        '''Invoke callback asynchronously (for both sync and async functions)'''
        if asyncio.iscoroutinefunction(callback):
            await callback(*args)
        else:
            callback(*args)

