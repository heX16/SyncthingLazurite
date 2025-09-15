unit AsyncHTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fphttpclient, ghashmap, HashMapStr, gdeque;

type
  // Operation status for CheckOperation
  TOperationState = (
    osNotFound,   // Operation not found (never existed or already cleaned up)
    osCreated,    // Operation just created (empty object)
    osQueued,     // Operation enqueued and waiting in queue
    osProcessing, // Operation picked up by worker and being processed
    osDone        // Operation finished, callback is pending/not yet cleaned up
  );

  // Internal kind of event to invoke in main thread
  TInvokeEventKind = (
    iekNone,
    iekOpened,
    iekSuccess,
    iekError,
    iekDisconnected,
    iekQueueEmpty
  );

  // Forward declaration
  TAsyncHTTP = class;
  THttpRequest = class;

  // Callback signature
  THttpRequestCallbackFunction = procedure(Request: THttpRequest) of object;
  THttpRequestCallbackFunctionStatic = procedure(Request: THttpRequest);

  // Request object passed to user callback
  THttpRequest = class
  public
    // HTTP response status code (0 if not available due to error)
    Status: Integer;
    // Response stream containing server data (position reset to 0 before callback)
    Response: TMemoryStream;
    // True if connection and request succeeded, false otherwise
    // TODO: rename or remove it
    Succeeded: Boolean;
    // Request URL for reference
    Url: string;
    // HTTP method (GET/POST/...) for reference
    HTTPMethod: string;
    // Operation state for this request lifecycle
    State: TOperationState;
    // Raw HTTP headers to be applied before sending
    HeadersRaw: string;
    // Optional request body for POST/other methods
    Data: string;
    // User callback to be invoked in main thread
    Callback: THttpRequestCallbackFunction;
    // Optional operation name for external tracking
    OperationName: string;
    // User-provided data (not owned by this class)
    UserObject: TObject;
    // User-provided string
    UserString: string;
    constructor Create;
    destructor Destroy; override;
  end;

  // Event signature providing the request first, then sender (owner)
  TAsyncHttpRequestEvent = procedure(Request: THttpRequest; Sender: TObject) of object;

  // Dictionary mapping operation name to request instance
  TDictHttpRequest = specialize THashMap<string, THttpRequest, THashFuncString>;

  // FIFO deque for requests
  TDequeHttpRequest = specialize TDeque<THttpRequest>;

  // Worker thread processing queued HTTP requests

  { TRequestWorkerThread }

  TRequestWorkerThread = class(TThread)
  private
    FOwner: TAsyncHTTP;
    FInvokeRequest: THttpRequest;
    FInvokeCallbackProc: THttpRequestCallbackFunction;
    FInvokeKind: TInvokeEventKind;
    procedure DoInvokeCallback;
    procedure DoInvokeEvents;
    function PerformSingleHttpAttempt(ARequest: THttpRequest; Client: TFPHTTPClient): Boolean;
    procedure ProcessRequest(ARequest: THttpRequest; Client: TFPHTTPClient = nil); virtual;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TAsyncHTTP);
  end;

  // HTTP client that never raises on HTTP response codes
  // client that does not raise on non-2xx/3xx codes
  TQuietHTTPClient = class(TFPHTTPClient)
  protected
    function CheckResponseCode(ACode: Integer; const AllowedResponseCodes: array of Integer): Boolean; override;
  end;

  // Abortable HTTP client to allow forcing socket close from outside
  TAbortableHTTPClient = class(TQuietHTTPClient)
  public
    procedure AbortRequest; virtual;
  end;

  // Main class exposed to users

  { TAsyncHTTP }

  TAsyncHTTP = class
  private
    FConnectTimeout: Integer;
    FQueue: TDequeHttpRequest;
    // Lock access to `FQueue` and `free(Request)`
    FQueueAndFreeLock: TCriticalSection;
    FQueueEvent: TEvent;
    FWorker: TRequestWorkerThread;
    FNamedRequestsDict: TDictHttpRequest;
    FNamedRequestsLock: TCriticalSection;
    FOnOpened: TAsyncHttpRequestEvent;
    FOnLoadDone: TAsyncHttpRequestEvent;
    FOnError: TAsyncHttpRequestEvent;
    FOnDisconnected: TAsyncHttpRequestEvent;
    FOnQueueEmpty: TNotifyEvent;
    FRetryCount: Integer;
    FIOTimeout: Integer;
    FTerminated: Boolean;
    FKeepConnection: Boolean;
    // Track current in-flight client for hard abort
    FCurrentClient: TFPHTTPClient;
    FCurrentClientLock: TCriticalSection;
    // True while a request is actively being processed
    FIsProcessing: Boolean;
    procedure EnqueueRequest(const ARequest: THttpRequest; const ClearDuplicates: Boolean = False);
    function GetRequestByName(const OperationName: string): THttpRequest;
    procedure AddRequestName(const ARequest: THttpRequest);
    procedure RemoveRequestName(const OperationName: string);
    // Remove queued requests matching the given operation name
    procedure CleanQueueByOperationName(const OperationName: string);
  protected
    // Factory method for HTTP client instance
    function CreateHttpClient: TFPHTTPClient; virtual;
    // Configure HTTP client based on timeouts and request-specific headers/body hints
    procedure ConfigureHttpClient(AClient: TFPHTTPClient; const ARequest: THttpRequest); virtual;
    // Cleanup HTTP client between requests/attempts (preparing for reuse)
    procedure CleanupHttpClient(AClient: TFPHTTPClient); virtual;
    // Factory for worker thread (allows customization in descendants)
    function CreateWorkerThread: TRequestWorkerThread; virtual;

    procedure AbortRequest(AClient: TFPHTTPClient); virtual;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Get(url: string;
      callback: THttpRequestCallbackFunction;
      headers: string = '';
      operationName: string = '';
      userObject: TObject = nil;
      userString: string = '';
      clearDuplicates: Boolean = False);

    procedure Post(url: string;
      data: string; callback:
      THttpRequestCallbackFunction;
      headers: string = '';
      operationName: string = '';
      userObject: TObject = nil;
      userString: string = '';
      clearDuplicates: Boolean = False);

    procedure HttpMethod(
      method: string;
      url: string;
      callback: THttpRequestCallbackFunction;
      headers: string = '';
      data: string = '';
      operationName: string = '';
      userObject: TObject = nil;
      userString: string = '';
      clearDuplicates: Boolean = False);

    // Graceful shutdown
    procedure Terminate();

    // Abort only the current in-flight connection (if any), without destroying the object
    // 'OnError' is called with `Request.Status = HTTPErrorCode_ClientClosed`
    procedure AbortActiveConnection();

    // Clear all pending (queued) requests without destroying the instance
    procedure ClearQueue();

    // Abort the active connection (if any) and clear the queue
    // 'OnError' is called with `Request.Status = HTTPErrorCode_ClientClosed`
    procedure CancelAll();

    // Check operation by name
    function RequestInQueue(const OperationName: string): Boolean;

    // Check operation is present and still queued (not processing)
    // Return true only if request is still queued and not in processing
    function RequestInQueueCold(const OperationName: string): Boolean;

    // Get current number of queued requests (excluding nil gaps)
    function QueueCount(): SizeUInt;

    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property IOTimeout: Integer read FIOTimeout write FIOTimeout;
    // Automatic retry on failure
    property RetryCount: Integer read FRetryCount write FRetryCount;
    // When true, reuse one HTTP client (keep-alive) within the worker thread
    property KeepConnection: Boolean read FKeepConnection write FKeepConnection;
    property OnOpened: TAsyncHttpRequestEvent read FOnOpened write FOnOpened;
    property OnLoadDone: TAsyncHttpRequestEvent read FOnLoadDone write FOnLoadDone;
    property OnError: TAsyncHttpRequestEvent read FOnError write FOnError;
    // Fired when low-level connection was interrupted and no HTTP status code received
    property OnDisconnected: TAsyncHttpRequestEvent read FOnDisconnected write FOnDisconnected;
    // Fired when the queue becomes empty after processing requests
    property OnQueueEmpty: TNotifyEvent read FOnQueueEmpty write FOnQueueEmpty;
    // True while a request is actively being processed
    property IsProcessing: Boolean read FIsProcessing;
  end;

const
  // Client Closed Request
  HTTPErrorCode_ClientClosed = 499;

implementation

{ TQuietHTTPClient }

function TQuietHTTPClient.CheckResponseCode(ACode: Integer; const AllowedResponseCodes: array of Integer): Boolean;
begin
  // Always treat any HTTP response code as acceptable to prevent exceptions.
  Result := True;
end;

{ TAbortableHTTPClient }

procedure TAbortableHTTPClient.AbortRequest;
begin
  // Force disconnect to break any blocking read/write and flag terminated
  DisconnectFromServer;
  Terminate;
end;

{ TAsyncHTTP - virtual factory/configuration }

function TAsyncHTTP.CreateHttpClient: TFPHTTPClient;
begin
  Result := TQuietHTTPClient.Create(nil);
end;

procedure TAsyncHTTP.ConfigureHttpClient(AClient: TFPHTTPClient; const ARequest: THttpRequest);
  procedure ApplyHeaders(AClientInner: TFPHTTPClient; const HeadersRaw: string);
  var
    lines: TStringList;
    i, p: Integer;
    line, key, value: string;
  begin
    if HeadersRaw = '' then Exit;
    lines := TStringList.Create;
    try
      lines.Text := HeadersRaw;
      for i := 0 to lines.Count - 1 do
      begin
        line := Trim(lines[i]);
        if line = '' then Continue;
        p := Pos(':', line);
        if p <= 0 then Continue;
        key := Trim(Copy(line, 1, p - 1));
        value := Trim(Copy(line, p + 1, Length(line)));
        if (key <> '') then
          AClientInner.AddHeader(key, value);
      end;
    finally
      lines.Free;
    end;
  end;
begin
  // AClient.AddHeader('Connection', 'close');
  AClient.AllowRedirect := True;
  AClient.KeepConnection := self.KeepConnection;
  
  if Self.FConnectTimeout > 0 then
    AClient.ConnectTimeout := Self.FConnectTimeout;
  if Self.FIOTimeout > 0 then
    AClient.IOTimeout := Self.FIOTimeout
  else if Self.FConnectTimeout > 0 then
    AClient.IOTimeout := Self.FConnectTimeout;
  ApplyHeaders(AClient, ARequest.HeadersRaw);
end;

procedure TAsyncHTTP.CleanupHttpClient(AClient: TFPHTTPClient);
begin
  // Reset per-request state to prepare for reuse.
  // Note: keep baseline defaults (Connection/Redirect/KeepConnection) intact.
  AClient.RequestHeaders.Clear;
  AClient.Cookies.Clear;
  AClient.RequestBody := nil;
  // Timeouts will be re-applied in ConfigureHttpClient before use.
end;

function TAsyncHTTP.CreateWorkerThread: TRequestWorkerThread;
begin
  Result := TRequestWorkerThread.Create(Self);
end;

procedure TAsyncHTTP.AbortRequest(AClient: TFPHTTPClient);
begin
  // Force disconnect to break any blocking read/write and flag terminated
  AClient.Terminate();
end;

{ THttpRequest }

constructor THttpRequest.Create;
begin
  inherited Create;

  Self.Url:='';
  Self.HTTPMethod:='';
  Self.HeadersRaw:='';
  Self.Data:='';
  Self.OperationName:='';
  Self.UserString:='';

  Self.Status := 0;
  Self.Response := nil;
  Self.Callback := nil;
  Self.Succeeded := False;
  Self.State := osCreated;
  Self.UserObject := nil;
end;

destructor THttpRequest.Destroy;
begin
  Self.Url:='';
  Self.HTTPMethod:='';
  Self.HeadersRaw:='';
  Self.Data:='';
  Self.OperationName:='';
  Self.UserString:='';
  FreeAndNil(Self.Response);
  inherited Destroy;
end;

{ TRequestWorkerThread }

constructor TRequestWorkerThread.Create(Owner: TAsyncHTTP);
begin
  inherited Create(True);
  Self.FreeOnTerminate := False;
  Self.FOwner := Owner;
  Self.FInvokeRequest := nil;
  Self.FInvokeCallbackProc := nil;
  Self.FInvokeKind := iekNone;
  Self.Start;
end;

procedure TRequestWorkerThread.DoInvokeEvents;
begin
  // Execute requested events in the main thread
  case Self.FInvokeKind of
    iekOpened:
      if Assigned(Self.FOwner.FOnOpened) then
        Self.FOwner.FOnOpened(Self.FInvokeRequest, Self.FOwner);
    iekSuccess:
      if Assigned(Self.FOwner.FOnLoadDone) then
        Self.FOwner.FOnLoadDone(Self.FInvokeRequest, Self.FOwner);
    iekError:
      if Assigned(Self.FOwner.FOnError) then
        Self.FOwner.FOnError(Self.FInvokeRequest, Self.FOwner);
    iekDisconnected:
      if Assigned(Self.FOwner.FOnDisconnected) then
        Self.FOwner.FOnDisconnected(Self.FInvokeRequest, Self.FOwner);
    iekQueueEmpty:
      if Assigned(Self.FOwner.FOnQueueEmpty) then
        Self.FOwner.FOnQueueEmpty(Self.FOwner);
  else
    ;
  end;
end;

procedure TRequestWorkerThread.DoInvokeCallback;
begin
  // Execute callback in the main/UI thread
  if Assigned(Self.FInvokeCallbackProc) and Assigned(Self.FInvokeRequest) then
    Self.FInvokeCallbackProc(Self.FInvokeRequest);
end;

function TRequestWorkerThread.PerformSingleHttpAttempt(ARequest: THttpRequest;
  Client: TFPHTTPClient): Boolean;
var
  body: TStream;
begin
  // Execute one HTTP attempt; return True to break retry loop
  if ARequest.Response = nil then
    ARequest.Response := TMemoryStream.Create
  else
    ARequest.Response.Size := 0;
  ARequest.Response.Position := 0;

  try
    // Always use HTTPMethod to reduce duplication (GET/POST wrappers call it)
    body := nil;
    if ARequest.Data <> '' then
    begin
      body := TStringStream.Create(ARequest.Data);
      Client.RequestBody := body;
      Client.RequestBody.Position := 0;
    end;
    try
      Client.HTTPMethod(ARequest.HTTPMethod, ARequest.Url, ARequest.Response, []);
    finally
      Client.RequestBody := nil;
      if body <> nil then
        FreeAndNil(body);
    end;

    ARequest.Status := Client.ResponseStatusCode;
    ARequest.Succeeded := (ARequest.Status >= 200) and (ARequest.Status < 400);
    if ARequest.Succeeded then
    begin
      Result := True; // don't retry
      Exit;
    end;
  except
    // Explicitly classify HTTP client errors caused by abort/terminate
    on E: EHTTPClient do
    begin
      ARequest.Succeeded := False;
      if (Client.Terminated) then
      begin
        // Treat as client-cancelled request
        ARequest.Status := HTTPErrorCode_ClientClosed; // Client Closed Request (non-standard)
        Result := True; // don't retry on intentional abort
        Exit;
      end
      else
      begin
        ARequest.Status := 0; // unknown HTTP error, allow retry
      end;
    end;
    // Other exceptions: mark as failure and allow retry attempts
    on E: Exception do
    begin
      ARequest.Succeeded := False;
      ARequest.Status := 0;
    end;
  end; // try

  Result := False; // do retry
end;

procedure TRequestWorkerThread.ProcessRequest(ARequest: THttpRequest;
  Client: TFPHTTPClient);
var
  attempt: Integer;
  clientNeedFree: Boolean;
begin
  // Mark started and fire OnOpened on the main thread
  Self.FOwner.FIsProcessing := True;
  Self.FInvokeKind := iekOpened;
  Self.FInvokeRequest := ARequest;
  Self.Synchronize(@DoInvokeEvents);

  ARequest.HTTPMethod := UpperCase(ARequest.HTTPMethod);

  clientNeedFree := False;
  if Client = nil then
  begin
    Client := Self.FOwner.CreateHttpClient();
    clientNeedFree := True;
  end
  else
    Self.FOwner.CleanupHttpClient(Client);

  try
    Self.FOwner.ConfigureHttpClient(Client, ARequest);

    // Expose current client for external abort
    Self.FOwner.FCurrentClientLock.Acquire;
    try
      Self.FOwner.FCurrentClient := Client;
    finally
      Self.FOwner.FCurrentClientLock.Release;
    end;

    // Do request
    for attempt := 0 to Self.FOwner.FRetryCount do
    begin
      if Self.PerformSingleHttpAttempt(ARequest, Client) then
        Break;
    end; // for

  finally
    // Clear current client reference
    Self.FOwner.FCurrentClientLock.Acquire;
    try
      if Self.FOwner.FCurrentClient = Client then
        Self.FOwner.FCurrentClient := nil;
    finally
      Self.FOwner.FCurrentClientLock.Release;
    end;

    if clientNeedFree then
      Client.Free;
  end; // try

  // Mark as done before invoking callback; after callback we will remove operation
  ARequest.State := osDone;

  // Fire success/error event in main thread
  if ARequest.Succeeded then
    Self.FInvokeKind := iekSuccess
  else
  begin
    // If there is no HTTP status and not connected, classify as disconnection
    if (ARequest.Status = 0) and (not ARequest.Succeeded) then
      Self.FInvokeKind := iekDisconnected
    else
      Self.FInvokeKind := iekError;
  end;
  Self.FInvokeRequest := ARequest;
  Self.Synchronize(@DoInvokeEvents);

  // Prepare and invoke the user callback in main thread
  ARequest.Response.Position := 0;
  Self.FInvokeRequest := ARequest;
  Self.FInvokeCallbackProc := ARequest.Callback;
  Self.Synchronize(@DoInvokeCallback);

  // After callback returns, cleanup

  // Remove request name from dictionary if it's the last request with this name
  if Self.FOwner.GetRequestByName(ARequest.OperationName) = ARequest then
    Self.FOwner.RemoveRequestName(ARequest.OperationName);

  Self.FOwner.FQueueAndFreeLock.Acquire;
  try
    FreeAndNil(ARequest);
  finally
    Self.FOwner.FQueueAndFreeLock.Release;
  end;
  Self.FOwner.FIsProcessing := False;
end;

procedure TRequestWorkerThread.Execute;
var
  req: THttpRequest;
  sharedClient: TFPHTTPClient;

  function GetRequestFromQueue(): THttpRequest;
  begin
    Result := nil;
    Self.FOwner.FQueueAndFreeLock.Acquire;
    try
      if Self.FOwner.FQueue.Size() > 0 then
      begin
        Result := Self.FOwner.FQueue.Front();
        Self.FOwner.FQueue.PopFront();
      end;
    finally
      Self.FOwner.FQueueAndFreeLock.Release;
    end;
  end;

begin
  sharedClient := nil;

  while not Self.Terminated do
  begin
    // Wait
    Self.FOwner.FQueueEvent.WaitFor(INFINITE);

    if Self.Terminated then
      Break;

    // Pop and process all available requests
    while Self.FOwner.QueueCount() > 0 do
    begin
      if Self.Terminated then
        Break;
      req := GetRequestFromQueue();
      if req = nil then
        Break;
      req.State := osProcessing;
      if Self.FOwner.FKeepConnection and (sharedClient = nil) then
        sharedClient := Self.FOwner.CreateHttpClient();
      Self.ProcessRequest(req, sharedClient);
    end; // while

    // Queue is empty - notify
    if Self.FOwner.QueueCount() > 0 then
    begin
      Self.FInvokeKind := iekQueueEmpty;
      Self.FInvokeRequest := nil;
      Self.Synchronize(@DoInvokeEvents);
    end;
  end;

  // Cleanup shared client if used
  if sharedClient <> nil then
    FreeAndNil(sharedClient);
end;

{ TAsyncHTTP }

constructor TAsyncHTTP.Create;
begin
  inherited Create;
  Self.FConnectTimeout := 0;
  Self.FRetryCount := 0;
  Self.FIOTimeout := 0;
  Self.FTerminated := False;
  Self.FKeepConnection := False;
  Self.FQueue := TDequeHttpRequest.Create;
  Self.FQueueAndFreeLock := TCriticalSection.Create;
  // Auto-reset event, initially non-signaled
  Self.FQueueEvent := TEvent.Create(nil, False, False, '');
  Self.FNamedRequestsDict := TDictHttpRequest.Create;
  Self.FNamedRequestsLock := TCriticalSection.Create;
  Self.FWorker := Self.CreateWorkerThread;
  Self.FCurrentClient := nil;
  Self.FCurrentClientLock := TCriticalSection.Create;
end;

destructor TAsyncHTTP.Destroy;
begin
  Self.Terminate;
  inherited Destroy;
end;

procedure TAsyncHTTP.EnqueueRequest(const ARequest: THttpRequest; const ClearDuplicates: Boolean);
begin
  // Map operation name to request if provided
  if ARequest.OperationName <> '' then
  begin
    if ClearDuplicates then
      Self.CleanQueueByOperationName(ARequest.OperationName);

    Self.AddRequestName(ARequest);
  end;

  Self.FQueueAndFreeLock.Acquire;
  try
    ARequest.State := osQueued;
    Self.FQueue.PushBack(ARequest);
    // Do not free ARequest here; ownership passes to the queue/worker
  finally
    Self.FQueueAndFreeLock.Release;
  end;

  // Wake worker - we have some work for him
  Self.FQueueEvent.SetEvent;
end;

function TAsyncHTTP.GetRequestByName(const OperationName: string): THttpRequest;
var
  req: THttpRequest;
begin
  Result := nil;
  if OperationName = '' then Exit;
  Self.FNamedRequestsLock.Acquire;
  try
    if Self.FNamedRequestsDict.GetValue(OperationName, req) then
      Result := req
    else
      Result := nil;
  finally
    Self.FNamedRequestsLock.Release;
  end;
end;

procedure TAsyncHTTP.AddRequestName(const ARequest: THttpRequest);
begin
  if (ARequest = nil) or (ARequest.OperationName = '') then Exit;

  Self.FNamedRequestsLock.Acquire;
  try
    // replace existing previous request with the same name
    Self.FNamedRequestsDict[ARequest.OperationName] := ARequest;
  finally
    Self.FNamedRequestsLock.Release;
  end;
end;

procedure TAsyncHTTP.RemoveRequestName(const OperationName: string);
begin
  if OperationName = '' then Exit;
  if Self.GetRequestByName(OperationName) = nil then Exit;

  Self.FNamedRequestsLock.Acquire;
  try
    Self.FNamedRequestsDict.delete(OperationName);
  finally
    Self.FNamedRequestsLock.Release;
  end;
end;

procedure TAsyncHTTP.CleanQueueByOperationName(const OperationName: string);
var
  i: SizeUInt;
  req: THttpRequest;
begin
  if OperationName = '' then Exit;
  if Self.GetRequestByName(OperationName) = nil then Exit;

  Self.FQueueAndFreeLock.Acquire;
  try
    // Remove name from mapping; it will be re-added for a new request
    RemoveRequestName(OperationName);

    // Remove all matching queued requests and compact the deque using Erase
    i := 0;
    while i < Self.FQueue.Size() do
    begin
      req := Self.FQueue.Items[i];
      if (req.OperationName = OperationName) and (req.State = osQueued) then
      begin
        FreeAndNil(req);
        Self.FQueue.Erase(i);
        Continue; // do not increment i, as items shifted left
      end;
      Inc(i);
    end;
  finally
    Self.FQueueAndFreeLock.Release;
  end;
end;

procedure TAsyncHTTP.Get(url: string; callback: THttpRequestCallbackFunction; headers: string; operationName: string; userObject: TObject; userString: string; clearDuplicates: Boolean);
var
  req: THttpRequest;
begin
  req := THttpRequest.Create;
  req.HTTPMethod := 'GET';
  req.Url := url;
  req.HeadersRaw := headers;
  req.Data := '';
  req.Callback := callback;
  req.OperationName := operationName;
  req.UserObject := userObject;
  req.UserString := userString;
  Self.EnqueueRequest(req, clearDuplicates);
end;

procedure TAsyncHTTP.Post(url: string; data: string; callback: THttpRequestCallbackFunction; headers: string; operationName: string; userObject: TObject; userString: string; clearDuplicates: Boolean);
var
  req: THttpRequest;
begin
  req := THttpRequest.Create;
  req.HTTPMethod := 'POST';
  req.Url := url;
  req.HeadersRaw := headers;
  req.Data := data;
  req.Callback := callback;
  req.OperationName := operationName;
  req.UserObject := userObject;
  req.UserString := userString;
  Self.EnqueueRequest(req, clearDuplicates);
end;

procedure TAsyncHTTP.HttpMethod(method: string; url: string; callback: THttpRequestCallbackFunction; headers: string; data: string; operationName: string; userObject: TObject; userString: string; clearDuplicates: Boolean);
var
  req: THttpRequest;
begin
  req := THttpRequest.Create;
  req.HTTPMethod := UpperCase(Trim(method));
  if req.HTTPMethod = '' then req.HTTPMethod := 'GET';
  req.Url := url;
  req.HeadersRaw := headers;
  req.Data := data;
  req.Callback := callback;
  req.OperationName := operationName;
  req.UserObject := userObject;
  req.UserString := userString;
  Self.EnqueueRequest(req, clearDuplicates);
end;

procedure TAsyncHTTP.Terminate;
var
  req: THttpRequest;
begin
  if Self.FTerminated then Exit;
  Self.FTerminated := True;
  // Self.Pause := True; // WIP
  
  AbortActiveConnection();

  if Assigned(Self.FWorker) then
  begin
    Self.FWorker.Terminate;
    if Assigned(Self.FQueueEvent) then
      Self.FQueueEvent.SetEvent;
    Self.FWorker.WaitFor;
    FreeAndNil(Self.FWorker);
  end;

  Self.FQueueAndFreeLock.Acquire;
  try
    while (Self.FQueue.Size() > 0) do
    begin
      req := Self.FQueue.Front();
      Self.FQueue.PopFront();
      if Assigned(req) and (req.State = osQueued) then
        FreeAndNil(req);
    end;
  finally
    Self.FQueueAndFreeLock.Release;
  end;

  FreeAndNil(Self.FQueueEvent);
  FreeAndNil(Self.FNamedRequestsDict);
  FreeAndNil(Self.FQueue);
  FreeAndNil(Self.FQueueAndFreeLock);
  FreeAndNil(Self.FNamedRequestsLock);
  FreeAndNil(Self.FCurrentClientLock);
end;

procedure TAsyncHTTP.AbortActiveConnection;
begin
  // Abort active client/socket if present
  Self.FCurrentClientLock.Acquire;
  try
    if Assigned(Self.FCurrentClient) then
    begin
      self.AbortRequest(Self.FCurrentClient);
    end;
  finally
    Self.FCurrentClientLock.Release;
  end;
end;

procedure TAsyncHTTP.ClearQueue;
var
  i: SizeUInt;
  req: THttpRequest;
  oldQueue: TDequeHttpRequest;
  oldDict: TDictHttpRequest;
begin
  // Atomically swap out queue and names under their respective locks
  oldQueue := nil;
  oldDict := nil;

  // Deattach queue and names from the instance
  Self.FQueueAndFreeLock.Acquire;
  Self.FNamedRequestsLock.Acquire;
  try
    if Self.FQueue.Size() > 0 then
    begin
      oldQueue := Self.FQueue;
      Self.FQueue := TDequeHttpRequest.Create;
    end;
    if Self.FNamedRequestsDict.Size() > 0 then
    begin
      oldDict := Self.FNamedRequestsDict;
      Self.FNamedRequestsDict := TDictHttpRequest.Create;
    end;
  finally
    Self.FNamedRequestsLock.Release;
    Self.FQueueAndFreeLock.Release;
  end;

  // Now safely free old containers outside locks
  if Assigned(oldQueue) then
  begin
    if oldQueue.Size() > 0 then
      for i := 0 to oldQueue.Size() - 1 do
      begin
        req := oldQueue.Items[i];
        if (req <> nil) and (req.State = osQueued) then
        begin
          FreeAndNil(req);
        end;
      end;
    oldQueue.Free;
  end;

  if Assigned(oldDict) then
    oldDict.Free;
end;

procedure TAsyncHTTP.CancelAll;
begin
  // Abort the current connection and clear queued requests
  Self.AbortActiveConnection;
  Self.ClearQueue;
end;

function TAsyncHTTP.RequestInQueue(const OperationName: string): Boolean;
begin
  Result := GetRequestByName(OperationName) <> nil;
end;

function TAsyncHTTP.RequestInQueueCold(const OperationName: string): Boolean;
var
  req: THttpRequest;
begin
  Self.FQueueAndFreeLock.Acquire;
  req := GetRequestByName(OperationName);
  Result := (req <> nil) and (req.State = osQueued);
  Self.FQueueAndFreeLock.Release;
end;

function TAsyncHTTP.QueueCount: SizeUInt;
begin
  Self.FQueueAndFreeLock.Acquire;
  Result := Self.FQueue.Size();
  Self.FQueueAndFreeLock.Release;
end;

end.


