'''
AsyncHTTP - Asynchronous HTTP request queue with callback support

This is a Python port of the Pascal AsyncHTTP.pas module.
Uses aiohttp for HTTP operations while maintaining the same API surface.

Threading Model:
- Uses asyncio for asynchronous operations
- Callbacks can be both sync and async functions
- All callbacks are executed in the event loop thread
'''

import asyncio
import aiohttp
from typing import Optional, Callable, Any, Dict
from enum import Enum
from io import BytesIO
import traceback


# Operation status for check_operation
class OperationState(Enum):
    '''Operation lifecycle states'''
    NOT_FOUND = 'osNotFound'       # Operation not found (never existed or already cleaned up)
    CREATED = 'osCreated'          # Operation just created (empty object)
    QUEUED = 'osQueued'            # Operation enqueued and waiting in queue
    PROCESSING = 'osProcessing'    # Operation picked up by worker and being processed
    DONE = 'osDone'                # Operation finished, callback is pending/not yet cleaned up


# Internal kind of event to invoke
class InvokeEventKind(Enum):
    '''Internal event types for callbacks'''
    NONE = 'iekNone'
    BEGIN_PROCESSING = 'iekBeginProcessing'
    SUCCESS = 'iekSuccess'
    ERROR = 'iekError'
    DISCONNECTED = 'iekDisconnected'
    QUEUE_EMPTY = 'iekQueueEmpty'


# HTTP Error codes (matching Pascal version)
HTTPErrorCode_ClientClosed = 16499
HTTPErrorCode_UnknownException = 16001
HTTPErrorCode_HTTPClientException = 16002
HTTPErrorCode_Disconnected = 16003
HTTPErrorCode_KeepAliveEnded = 16004
HTTPErrorCode_SocketHostNotFound = 16005
HTTPErrorCode_SocketCreationFailed = 16006
HTTPErrorCode_SocketConnectFailed = 16007
HTTPErrorCode_SocketConnectTimeout = 16008
HTTPErrorCode_SocketIOTimeout = 16009
HTTPErrorCode_HTTPClientSocketError = 16010


class HttpRequest:
    '''
    Request object passed to user callback
    
    Matches the THttpRequest class from Pascal version
    '''
    
    def __init__(self):
        # HTTP response status code (0 if not available due to error)
        self.Status: int = 0
        
        # Response stream containing server data (position reset to 0 before callback)
        self.Response: Optional[BytesIO] = None
        
        # True if connection and request succeeded, false otherwise
        self.Succeeded: bool = False
        
        # Request URL for reference
        self.Url: str = ''
        
        # HTTP method (GET/POST/...) for reference
        self.HTTPMethod: str = ''
        
        # Operation state for this request lifecycle
        self.State: OperationState = OperationState.CREATED
        
        # Raw HTTP headers as dictionary
        self.HeadersRaw: str = ''
        
        # Optional request body for POST/other methods
        self.Data: str = ''
        
        # User callback to be invoked
        self.Callback: Optional[Callable] = None
        
        # Optional operation name for external tracking
        self.OperationName: str = ''
        
        # User-provided data (not owned by this class)
        self.UserObject: Any = None
        
        # User-provided string
        self.UserString: str = ''
        
        # Internal: cancellation flag
        self._cancelled: bool = False


class AsyncHTTP:
    '''
    Main class for asynchronous HTTP requests with queue management
    
    THREADING MODEL:
    This class uses asyncio for HTTP operations. Callbacks are executed
    in the event loop thread.
    
    CALLBACK BEHAVIOR:
    - Request callback is ALWAYS called, even on errors
    - On error: Request.Status contains HTTPErrorCode_* and Request.Succeeded = False
    - On success: Request.Status contains HTTP status code and Request.Succeeded = True
    - OnBeginProcessing, OnError, OnLoadDone are global handlers called for ALL requests
    - Global handlers are optional - using individual request callbacks is sufficient
    
    EVENT FIRING ORDER:
    1. OnBeginProcessing is fired when request starts processing
    2. OnLoadDone is fired for successful responses (status 200-399)
       OnError is fired for HTTP error responses (status 400+ or connection errors)
    3. OnDisconnected is fired ADDITIONALLY when TCP connection is closed by server
    4. Individual request callback is fired last
    '''
    
    def __init__(self):
        # Timeout settings (in milliseconds, like Pascal version)
        self._connect_timeout: int = 0  # 0 means default (no timeout)
        self._io_timeout: int = 0       # 0 means default (no timeout)
        
        # Queue for requests
        self._queue: asyncio.Queue = asyncio.Queue()
        
        # Dictionary mapping operation name to request instance
        self._named_requests: Dict[str, HttpRequest] = {}
        
        # Lock for named requests dictionary
        self._named_requests_lock: asyncio.Lock = asyncio.Lock()
        
        # Event handlers
        self.OnBeginProcessing: Optional[Callable] = None
        self.OnLoadDone: Optional[Callable] = None
        self.OnError: Optional[Callable] = None
        self.OnDisconnected: Optional[Callable] = None
        self.OnQueueEmpty: Optional[Callable] = None
        
        # Retry settings
        self._retry_count: int = 0
        
        # Keep connection setting
        self._keep_connection: bool = False
        
        # Termination flag
        self._terminated: bool = False
        
        # Cancelling in process flag
        self._cancelling_in_process: bool = False
        
        # True while a request is actively being processed
        self._is_processing: bool = False
        
        # User-provided data
        self.UserObject: Any = None
        self.UserString: str = ''
        
        # Worker task
        self._worker_task: Optional[asyncio.Task] = None
        
        # Current session (for keep-alive)
        self._session: Optional[aiohttp.ClientSession] = None
        
        # Current request being processed (for cancellation)
        self._current_request: Optional[HttpRequest] = None
        
        # Current HTTP request task (for aborting long-polling)
        self._current_http_task: Optional[asyncio.Task] = None
        self._current_http_task_lock: asyncio.Lock = asyncio.Lock()
        
        # Start worker
        self._start_worker()
    
    def _start_worker(self):
        '''Start the worker task'''
        if self._worker_task is None or self._worker_task.done():
            self._worker_task = asyncio.create_task(self._worker_loop())
    
    async def _worker_loop(self):
        '''Main worker loop processing queued requests'''
        while not self._terminated:
            try:
                # Wait for request with timeout to check termination flag
                try:
                    request = await asyncio.wait_for(self._queue.get(), timeout=0.5)
                except asyncio.TimeoutError:
                    continue
                
                if request is None:  # Poison pill for shutdown
                    break
                
                # Skip cancelled requests (cleaned by ClearDuplicates)
                if request._cancelled:
                    continue
                
                # Process the request
                await self._process_request(request)
                
                # Check if queue is empty
                if self._queue.empty():
                    await self._invoke_event(InvokeEventKind.QUEUE_EMPTY, None)
                    
            except Exception as e:
                # Log unexpected errors in worker loop
                print(f'AsyncHTTP worker loop error: {e}')
                traceback.print_exc()
        
        # Cleanup session
        if self._session:
            await self._session.close()
            self._session = None
    
    async def _process_request(self, request: HttpRequest):
        '''Process a single HTTP request with retry logic'''
        # Double-check cancellation (might have been cancelled after dequeue)
        if request._cancelled:
            return
        
        self._is_processing = True
        self._current_request = request
        
        try:
            # Fire OnBeginProcessing
            await self._invoke_event(InvokeEventKind.BEGIN_PROCESSING, request)
            
            # Prepare session
            if self._keep_connection and self._session is None:
                self._session = aiohttp.ClientSession()
            elif not self._keep_connection:
                self._session = aiohttp.ClientSession()
            
            request.State = OperationState.PROCESSING
            
            # Perform request with retries
            # Note: Unlike Pascal version, we don't need special handling for broken
            # keep-alive sockets (HTTPErrorCode_KeepAliveEnded). The Pascal version
            # explicitly disconnects and retries (giving 2 + RetryCount attempts),
            # but aiohttp automatically manages connection pool and reconnects broken
            # keep-alive sockets internally. This simplifies our retry logic to just
            # 1 + RetryCount attempts for all error types.
            attempt = 0
            max_attempts = 1 + self._retry_count
            
            try:
                while attempt < max_attempts and not request._cancelled:
                    await self._perform_single_attempt(request)
                    
                    if request.Succeeded or request._cancelled:
                        break
                    
                    attempt += 1
            except asyncio.CancelledError:
                # Request was cancelled during attempt
                request.Status = HTTPErrorCode_ClientClosed
                request.Succeeded = False
                # Don't re-raise, continue to callback
            
            # Fire success or error event
            if request.Succeeded:
                await self._invoke_event(InvokeEventKind.SUCCESS, request)
            else:
                await self._invoke_event(InvokeEventKind.ERROR, request)
            
            # Fire disconnected event if needed
            # This fires when connection was closed (by server or by client abort)
            if request.Status in (HTTPErrorCode_Disconnected, 
                                 HTTPErrorCode_SocketConnectFailed,
                                 HTTPErrorCode_SocketIOTimeout,
                                 HTTPErrorCode_ClientClosed):  # Client-initiated abort
                await self._invoke_event(InvokeEventKind.DISCONNECTED, request)
            
            # Fire individual callback
            if request.Callback:
                await self._invoke_callback(request.Callback, request)
            
        finally:
            # Cleanup
            request.State = OperationState.DONE
            self._is_processing = False
            self._current_request = None
            
            # Remove from named requests ONLY if it's still the same request object
            # (not replaced by a newer request with same name)
            if request.OperationName:
                async with self._named_requests_lock:
                    # This check is critical: remove only if it's the exact same object
                    if self._named_requests.get(request.OperationName) is request:
                        del self._named_requests[request.OperationName]
            
            # Close session if not keeping connection
            if not self._keep_connection and self._session:
                await self._session.close()
                self._session = None
    
    async def _do_http_request(self, method: str, url: str, kwargs: dict) -> dict:
        '''
        Perform actual HTTP request
        Returns dict with 'status' and 'body' keys
        This is separated to allow task cancellation
        '''
        if self._session is None or self._session.closed:
            raise RuntimeError('HTTP session is not available')
        
        async with self._session.request(method, url, **kwargs) as response:
            status = response.status
            body = await response.read()
            return {'status': status, 'body': body}
    
    async def _perform_single_attempt(self, request: HttpRequest):
        '''Perform a single HTTP request attempt'''
        request.Status = 0
        
        if request.Response is None:
            request.Response = BytesIO()
        else:
            request.Response.seek(0)
            request.Response.truncate(0)
        
        try:
            # Parse headers
            headers = self._parse_headers(request.HeadersRaw)
            
            # Prepare timeout (convert from milliseconds to seconds)
            # Pascal version uses milliseconds, aiohttp uses seconds
            timeout = aiohttp.ClientTimeout(
                total=None,
                connect=self._connect_timeout / 1000.0 if self._connect_timeout > 0 else None,
                sock_read=self._io_timeout / 1000.0 if self._io_timeout > 0 else None
            )
            
            # Prepare request kwargs
            kwargs = {
                'headers': headers,
                'timeout': timeout,
                'allow_redirects': True
            }
            
            # Add data for POST/PUT/etc
            if request.Data:
                kwargs['data'] = request.Data
            
            # Create HTTP request as a task for cancellation support
            http_task = asyncio.create_task(
                self._do_http_request(request.HTTPMethod, request.Url, kwargs)
            )
            
            # Register task for abortion
            async with self._current_http_task_lock:
                self._current_http_task = http_task
            
            try:
                # Wait for HTTP request to complete
                response_data = await http_task
                request.Status = response_data['status']
                request.Response.write(response_data['body'])
                request.Response.seek(0)
                request.Succeeded = 200 <= request.Status < 400
            finally:
                # Unregister task
                async with self._current_http_task_lock:
                    if self._current_http_task is http_task:
                        self._current_http_task = None
                
        except asyncio.CancelledError:
            # Request was cancelled (AbortActiveConnection or CancelAll)
            request.Status = HTTPErrorCode_ClientClosed
            request.Succeeded = False
            request._cancelled = True
            raise  # Re-raise to stop retry loop
            
        except asyncio.TimeoutError:
            if self._cancelling_in_process or request._cancelled:
                request.Status = HTTPErrorCode_ClientClosed
            else:
                request.Status = HTTPErrorCode_SocketIOTimeout
            request.Succeeded = False
            
        except aiohttp.ClientConnectorError as e:
            # Connection errors
            request.Status = HTTPErrorCode_SocketConnectFailed
            request.Succeeded = False
            
        except aiohttp.ClientError as e:
            # Other aiohttp errors
            request.Status = HTTPErrorCode_HTTPClientException
            request.Succeeded = False
        
        except RuntimeError as e:
            # Session closed or other runtime errors
            if 'session' in str(e).lower():
                request.Status = HTTPErrorCode_ClientClosed
            else:
                request.Status = HTTPErrorCode_UnknownException
            request.Succeeded = False
            
        except Exception as e:
            # Unknown exceptions
            request.Status = HTTPErrorCode_UnknownException
            request.Succeeded = False
            print(f'AsyncHTTP unexpected error: {e}')
            traceback.print_exc()
    
    def _parse_headers(self, headers_raw: str) -> dict:
        '''Parse raw headers string into dictionary'''
        headers = {}
        if not headers_raw:
            return headers
        
        for line in headers_raw.split('\n'):
            line = line.strip()
            if not line:
                continue
            
            if ':' in line:
                key, value = line.split(':', 1)
                headers[key.strip()] = value.strip()
        
        return headers
    
    async def _invoke_event(self, kind: InvokeEventKind, request: Optional[HttpRequest]):
        '''Invoke event handler based on kind'''
        try:
            if kind == InvokeEventKind.BEGIN_PROCESSING and self.OnBeginProcessing:
                await self._invoke_callback(self.OnBeginProcessing, request, self)
            elif kind == InvokeEventKind.SUCCESS and self.OnLoadDone:
                await self._invoke_callback(self.OnLoadDone, request, self)
            elif kind == InvokeEventKind.ERROR and self.OnError:
                await self._invoke_callback(self.OnError, request, self)
            elif kind == InvokeEventKind.DISCONNECTED and self.OnDisconnected:
                await self._invoke_callback(self.OnDisconnected, request, self)
            elif kind == InvokeEventKind.QUEUE_EMPTY and self.OnQueueEmpty:
                await self._invoke_callback(self.OnQueueEmpty, self)
        except Exception as e:
            print(f'Error in event handler {kind}: {e}')
            traceback.print_exc()
    
    async def _invoke_callback(self, callback: Callable, *args):
        '''Invoke callback, handling both sync and async functions'''
        try:
            if asyncio.iscoroutinefunction(callback):
                await callback(*args)
            else:
                callback(*args)
        except Exception as e:
            print(f'Error in callback: {e}')
            traceback.print_exc()
    
    async def _enqueue_request(self, request: HttpRequest, clear_duplicates: bool = False):
        '''Enqueue a request for processing'''
        # Clear cancelling flag
        self._cancelling_in_process = False
        
        # Handle operation name
        if request.OperationName:
            if clear_duplicates:
                await self._clean_queue_by_operation_name(request.OperationName)
            
            async with self._named_requests_lock:
                self._named_requests[request.OperationName] = request
        
        request.State = OperationState.QUEUED
        await self._queue.put(request)
    
    async def _clean_queue_by_operation_name(self, operation_name: str):
        '''Remove all queued requests with the given operation name'''
        if not operation_name:
            return
        
        async with self._named_requests_lock:
            # Mark the old request as cancelled so it won't be processed
            old_request = self._named_requests.get(operation_name)
            if old_request and old_request.State == OperationState.QUEUED:
                old_request._cancelled = True
            
            # Remove from dictionary (will be re-added with new request)
            if operation_name in self._named_requests:
                del self._named_requests[operation_name]
    
    # Public API methods
    
    def Get(self, url: str, callback: Optional[Callable] = None, 
            headers: str = '', operation_name: str = '',
            user_object: Any = None, user_string: str = '',
            clear_duplicates: bool = False):
        '''
        Queue a GET request
        
        Args:
            url: Request URL
            callback: Callback function to invoke when done
            headers: Raw headers string (one per line, "Key: Value")
            operation_name: Optional name for tracking
            user_object: User-provided object
            user_string: User-provided string
            clear_duplicates: Clear pending requests with same operation_name
        '''
        asyncio.create_task(self._get_async(
            url, callback, headers, operation_name, 
            user_object, user_string, clear_duplicates
        ))
    
    async def _get_async(self, url: str, callback: Optional[Callable],
                         headers: str, operation_name: str,
                         user_object: Any, user_string: str,
                         clear_duplicates: bool):
        '''Internal async implementation of Get'''
        request = HttpRequest()
        request.HTTPMethod = 'GET'
        request.Url = url
        request.HeadersRaw = headers
        request.Data = ''
        request.Callback = callback
        request.OperationName = operation_name
        request.UserObject = user_object
        request.UserString = user_string
        
        await self._enqueue_request(request, clear_duplicates)
    
    def Post(self, url: str, data: str, callback: Optional[Callable] = None,
             headers: str = '', operation_name: str = '',
             user_object: Any = None, user_string: str = '',
             clear_duplicates: bool = False):
        '''
        Queue a POST request
        
        Args:
            url: Request URL
            data: Request body
            callback: Callback function to invoke when done
            headers: Raw headers string (one per line, "Key: Value")
            operation_name: Optional name for tracking
            user_object: User-provided object
            user_string: User-provided string
            clear_duplicates: Clear pending requests with same operation_name
        '''
        asyncio.create_task(self._post_async(
            url, data, callback, headers, operation_name,
            user_object, user_string, clear_duplicates
        ))
    
    async def _post_async(self, url: str, data: str, callback: Optional[Callable],
                          headers: str, operation_name: str,
                          user_object: Any, user_string: str,
                          clear_duplicates: bool):
        '''Internal async implementation of Post'''
        request = HttpRequest()
        request.HTTPMethod = 'POST'
        request.Url = url
        request.HeadersRaw = headers
        request.Data = data
        request.Callback = callback
        request.OperationName = operation_name
        request.UserObject = user_object
        request.UserString = user_string
        
        await self._enqueue_request(request, clear_duplicates)
    
    def HttpMethod(self, method: str, url: str, callback: Optional[Callable] = None,
                   headers: str = '', data: str = '',
                   operation_name: str = '', user_object: Any = None,
                   user_string: str = '', clear_duplicates: bool = False):
        '''
        Queue a request with custom HTTP method
        
        Args:
            method: HTTP method (GET, POST, PUT, DELETE, etc)
            url: Request URL
            callback: Callback function to invoke when done
            headers: Raw headers string (one per line, "Key: Value")
            data: Request body
            operation_name: Optional name for tracking
            user_object: User-provided object
            user_string: User-provided string
            clear_duplicates: Clear pending requests with same operation_name
        '''
        asyncio.create_task(self._http_method_async(
            method, url, callback, headers, data, operation_name,
            user_object, user_string, clear_duplicates
        ))
    
    async def _http_method_async(self, method: str, url: str, callback: Optional[Callable],
                                 headers: str, data: str, operation_name: str,
                                 user_object: Any, user_string: str,
                                 clear_duplicates: bool):
        '''Internal async implementation of HttpMethod'''
        request = HttpRequest()
        request.HTTPMethod = method.upper().strip()
        if not request.HTTPMethod:
            request.HTTPMethod = 'GET'
        request.Url = url
        request.HeadersRaw = headers
        request.Data = data
        request.Callback = callback
        request.OperationName = operation_name
        request.UserObject = user_object
        request.UserString = user_string
        
        await self._enqueue_request(request, clear_duplicates)
    
    def AbortActiveConnection(self):
        '''
        Abort only the current in-flight connection (if any)
        OnError is called with Request.Status = HTTPErrorCode_ClientClosed
        '''
        asyncio.create_task(self._abort_active_connection_async())
    
    async def _abort_active_connection_async(self):
        '''Internal async implementation of AbortActiveConnection'''
        # Mark request as cancelled
        if self._current_request:
            self._current_request._cancelled = True
        
        # Cancel the current HTTP task (this interrupts long-polling immediately)
        async with self._current_http_task_lock:
            if self._current_http_task and not self._current_http_task.done():
                self._current_http_task.cancel()
                # Wait for cancellation to complete
                try:
                    await self._current_http_task
                except asyncio.CancelledError:
                    pass  # Expected
                except Exception:
                    pass  # Ignore other errors
    
    def ClearQueue(self):
        '''Clear all pending (queued) requests without destroying the instance'''
        asyncio.create_task(self._clear_queue_async())
    
    async def _clear_queue_async(self):
        '''Internal async implementation of ClearQueue'''
        # Atomically swap out queue and names (matching Pascal implementation)
        new_queue = asyncio.Queue()
        old_queue = self._queue
        self._queue = new_queue
        
        # Clear named requests and mark them as cancelled
        old_requests = []
        async with self._named_requests_lock:
            old_requests = list(self._named_requests.values())
            self._named_requests.clear()
        
        # Mark all old requests as cancelled
        for req in old_requests:
            if req.State == OperationState.QUEUED:
                req._cancelled = True
        
        # Drain old queue and mark requests as cancelled
        while not old_queue.empty():
            try:
                req = old_queue.get_nowait()
                if req and req.State == OperationState.QUEUED:
                    req._cancelled = True
            except asyncio.QueueEmpty:
                break
    
    def CancelAll(self):
        '''
        Abort the active connection (if any) and clear the queue
        OnError is called with Request.Status = HTTPErrorCode_ClientClosed
        '''
        asyncio.create_task(self._cancel_all_async())
    
    async def _cancel_all_async(self):
        '''Internal async implementation of CancelAll'''
        self._cancelling_in_process = True
        
        # Clear queue and abort connection in parallel
        await asyncio.gather(
            self._clear_queue_async(),
            self._abort_active_connection_async(),
            return_exceptions=True
        )
    
    def RequestInQueue(self, operation_name: str) -> bool:
        '''
        Check if operation exists by name
        
        Args:
            operation_name: Operation name to check
            
        Returns:
            True if operation exists in any state
        '''
        return operation_name in self._named_requests
    
    def RequestInQueueCold(self, operation_name: str) -> bool:
        '''
        Check if operation is present and still queued (not processing)
        
        Args:
            operation_name: Operation name to check
            
        Returns:
            True only if request is still queued and not processing
        '''
        request = self._named_requests.get(operation_name)
        if request:
            return request.State == OperationState.QUEUED
        return False
    
    def QueueCount(self) -> int:
        '''
        Get current number of queued requests
        
        Returns:
            Number of requests in queue
        '''
        return self._queue.qsize()
    
    # Properties (using Python property decorators for read/write)
    
    @property
    def ConnectTimeout(self) -> int:
        '''Connection timeout in milliseconds (matching Pascal version)'''
        return self._connect_timeout
    
    @ConnectTimeout.setter
    def ConnectTimeout(self, value: int):
        self._connect_timeout = value
    
    @property
    def IOTimeout(self) -> int:
        '''I/O timeout in milliseconds (matching Pascal version)'''
        return self._io_timeout
    
    @IOTimeout.setter
    def IOTimeout(self, value: int):
        self._io_timeout = value
    
    @property
    def RetryCount(self) -> int:
        '''Number of automatic retries on failure'''
        return self._retry_count
    
    @RetryCount.setter
    def RetryCount(self, value: int):
        self._retry_count = value
    
    @property
    def KeepConnection(self) -> bool:
        '''When True, reuse one HTTP client (keep-alive)'''
        return self._keep_connection
    
    @KeepConnection.setter
    def KeepConnection(self, value: bool):
        self._keep_connection = value
    
    @property
    def IsProcessing(self) -> bool:
        '''True while a request is actively being processed'''
        return self._is_processing
    
    def __del__(self):
        '''Destructor'''
        self._terminated = True
        
        # Queue poison pill to stop worker
        if self._queue:
            try:
                self._queue.put_nowait(None)
            except:
                pass
    
    async def Destroy(self):
        '''
        Async cleanup method
        Call this explicitly before program exit for clean shutdown
        '''
        self._terminated = True
        
        # Stop worker
        if self._worker_task:
            await self._queue.put(None)  # Poison pill
            try:
                await asyncio.wait_for(self._worker_task, timeout=2.0)
            except asyncio.TimeoutError:
                self._worker_task.cancel()
        
        # Close session
        if self._session:
            await self._session.close()
            self._session = None

