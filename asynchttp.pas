unit AsyncHTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fphttpclient, ghashmap, HashMapStr, gqueue;

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
    iekDisconnected
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
    Connected: Boolean;
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
    UserString: string;
    constructor Create;
    destructor Destroy; override;
  end;

  // Event signature providing the request first, then sender (owner)
  TAsyncHttpRequestEvent = procedure(Request: THttpRequest; Sender: TObject) of object;

  // Dictionary mapping operation name to request instance
  TDictHttpRequest = specialize THashMap<string, THttpRequest, THashFuncString>;

  // FIFO queue for requests
  TQueueHttpRequest = specialize TQueue<THttpRequest>;

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
    procedure ProcessRequest(ARequest: THttpRequest; Client: TFPHTTPClient = nil); virtual;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TAsyncHTTP);
  end;

  // HTTP client that never raises on HTTP response codes
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
    FQueue: TQueueHttpRequest;
    FQueueLock: TCriticalSection;
    FQueueEvent: TEvent;
    FWorker: TRequestWorkerThread;
    FNamedRequestsDict: TDictHttpRequest;
    FNamedRequestsLock: TCriticalSection;
    FOnOpened: TAsyncHttpRequestEvent;
    FOnLoadDone: TAsyncHttpRequestEvent;
    FOnError: TAsyncHttpRequestEvent;
    FOnDisconnected: TAsyncHttpRequestEvent;
    FRetryCount: Integer;
    FIOTimeout: Integer;
    FTerminated: Boolean;
    FKeepConnection: Boolean;
    // Track current in-flight client for hard abort
    FCurrentClient: TAbortableHTTPClient;
    FCurrentClientLock: TCriticalSection;
    // True when a request has been opened and not yet completed
    FIsConnectionOpen: Boolean;
    procedure EnqueueRequest(const ARequest: THttpRequest);
    function GetRequestByName(const OperationName: string): THttpRequest;
    procedure AddRequestName(const ARequest: THttpRequest);
    procedure RemoveRequestName(const OperationName: string);
  protected
    // Factory method for HTTP client instance
    function CreateHttpClient: TFPHTTPClient; virtual;
    // Configure HTTP client based on timeouts and request-specific headers/body hints
    procedure ConfigureHttpClient(AClient: TFPHTTPClient; const ARequest: THttpRequest); virtual;
    // Cleanup HTTP client between requests/attempts (preparing for reuse)
    procedure CleanupHttpClient(AClient: TFPHTTPClient); virtual;
    // Factory for worker thread (allows customization in descendants)
    function CreateWorkerThread: TRequestWorkerThread; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Get(url: string;
      callback: THttpRequestCallbackFunction;
      headers: string = '';
      operationName: string = '';
      userObject: TObject = nil;
      userString: string = '');

    procedure Post(url: string;
      data: string; callback:
      THttpRequestCallbackFunction;
      headers: string = '';
      operationName: string = '';
      userObject: TObject = nil;
      userString: string = '');

    procedure HttpMethod(
      method: string;
      url: string;
      callback: THttpRequestCallbackFunction;
      headers: string = '';
      data: string = '';
      operationName: string = '';
      userObject: TObject = nil;
      userString: string = '');

    // Graceful shutdown
    procedure Terminate;

    // Abort only the current in-flight connection (if any), without destroying the object
    procedure AbortActiveConnection;

    // Check operation status by name
    function RequestInQueue(const OperationName: string): Boolean;

    // Properties
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property IOTimeout: Integer read FIOTimeout write FIOTimeout;
    property RetryCount: Integer read FRetryCount write FRetryCount;
    // When true, reuse one HTTP client (keep-alive) within the worker thread
    property KeepConnection: Boolean read FKeepConnection write FKeepConnection;
    property OnOpened: TAsyncHttpRequestEvent read FOnOpened write FOnOpened;
    property OnLoadDone: TAsyncHttpRequestEvent read FOnLoadDone write FOnLoadDone;
    property OnError: TAsyncHttpRequestEvent read FOnError write FOnError;
    // Fired when low-level connection was interrupted and no HTTP status code received
    property OnDisconnected: TAsyncHttpRequestEvent read FOnDisconnected write FOnDisconnected;
    // True if a request is currently opened and in-flight
    property IsConnectionOpen: Boolean read FIsConnectionOpen;
  end;

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
  // Create client that does not raise on non-2xx/3xx codes
  Result := TAbortableHTTPClient.Create(nil);
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

{ THttpRequest }

constructor THttpRequest.Create;
begin
  inherited Create;
  Self.Status := 0;
  Self.Response := nil;
  Self.Connected := False;
  Self.State := osCreated;
  // Initialize user data pointers
  Self.UserObject := nil;
  Self.UserString := '';
end;

destructor THttpRequest.Destroy;
begin
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

procedure TRequestWorkerThread.Execute;
var
  req: THttpRequest;
  sharedClient: TFPHTTPClient;
  req_old_state: TOperationState;
begin
  sharedClient := nil;
  while not Self.Terminated do
  begin
    // Wait until there is work or termination
    Self.FOwner.FQueueEvent.WaitFor(INFINITE);
    if Self.Terminated then Break;

    // Pop and process all available requests
    while True do
    begin
      if Self.Terminated then Break;

      req := nil;

      Self.FOwner.FQueueLock.Acquire;
      try
        if Self.FOwner.FQueue.Size() > 0 then
        begin
          req := Self.FOwner.FQueue.Front();
          req_old_state := req.State;
          req.State := osProcessing;
          Self.FOwner.FQueue.Pop();
        end;
      finally
        Self.FOwner.FQueueLock.Release;
      end;

      if req = nil then Break;

      if (req <> nil) and (req_old_state <> osQueued) then
      begin
        continue;
      end;

      if Self.FOwner.FKeepConnection then
      begin
        if sharedClient = nil then
          sharedClient := Self.FOwner.CreateHttpClient;
        Self.ProcessRequest(req, sharedClient);
      end else
      begin
        Self.ProcessRequest(req, nil);
      end;
    end;
  end;

  // Cleanup shared client if used
  if sharedClient <> nil then
    sharedClient.Free;
end;

procedure TRequestWorkerThread.ProcessRequest(ARequest: THttpRequest;
  Client: TFPHTTPClient);
var
  attempt: Integer;
  succeeded: Boolean;
  body: TStream;
  clientNeedFree: Boolean;
begin
  // Mark started and fire OnOpened on the main thread
  Self.FInvokeKind := iekOpened;
  Self.FInvokeRequest := ARequest;
  Self.Synchronize(@DoInvokeEvents);
  Self.FOwner.FIsConnectionOpen := True;

  succeeded := False;
  ARequest.HTTPMethod := UpperCase(ARequest.HTTPMethod);

  clientNeedFree := False;
  if Client = nil then
  begin
    Client := Self.FOwner.CreateHttpClient;
    clientNeedFree := True;
  end
  else
    Self.FOwner.CleanupHttpClient(Client);

  try
    Self.FOwner.ConfigureHttpClient(Client, ARequest);

    // Expose current client for external abort
    if Client is TAbortableHTTPClient then
    begin
      Self.FOwner.FCurrentClientLock.Acquire;
      try
        Self.FOwner.FCurrentClient := TAbortableHTTPClient(Client);
      finally
        Self.FOwner.FCurrentClientLock.Release;
      end;
    end;

    for attempt := 0 to Self.FOwner.FRetryCount do
    begin
      if ARequest.Response = nil then
        ARequest.Response := TMemoryStream.Create
      else
        ARequest.Response.Size := 0;
      ARequest.Response.Position := 0;

      try
        if ARequest.HTTPMethod = 'GET' then
          Client.Get(ARequest.Url, ARequest.Response)
        else if ARequest.HTTPMethod = 'POST' then
        begin
          body := nil;
          if ARequest.Data <> '' then
          begin
            body := TStringStream.Create(ARequest.Data);
            Client.RequestBody := body;
            Client.RequestBody.Position := 0;
          end;
          try
            Client.Post(ARequest.Url, ARequest.Response);
          finally
            Client.RequestBody := nil;
            FreeAndNil(body);
          end;
        end
        else
        begin
          // Generic HTTP method
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
            FreeAndNil(body);
          end;
        end;

        ARequest.Status := Client.ResponseStatusCode;
        ARequest.Connected := (ARequest.Status >= 200) and (ARequest.Status < 400);
        if ARequest.Connected then
        begin
          succeeded := True;
          Break;
        end;
      except
        // Explicitly classify HTTP client errors caused by abort/terminate
        on E: EHTTPClient do
        begin
          ARequest.Connected := False;
          if (Client.Terminated or Self.FOwner.FTerminated) and
             (CompareText(E.Message, 'Error reading data from socket') = 0) then
          begin
            // Treat as client-cancelled request
            ARequest.Status := 499; // Client Closed Request (non-standard)
            Break; // don't retry on intentional abort
          end
          else
          begin
            ARequest.Status := 0; // unknown HTTP error, allow retry
          end;
        end;
        // Other exceptions: mark as failure and allow retry attempts
        on E: Exception do
        begin
          ARequest.Connected := False;
          ARequest.Status := 0;
        end;
      end;
    end;
  finally
    // Clear current client reference
    if Client is TAbortableHTTPClient then
    begin
      Self.FOwner.FCurrentClientLock.Acquire;
      try
        if Self.FOwner.FCurrentClient = Client then
          Self.FOwner.FCurrentClient := nil;
      finally
        Self.FOwner.FCurrentClientLock.Release;
      end;
    end;
    if clientNeedFree then
      Client.Free;
  end;

  // Mark as done before invoking callback; after callback we will remove operation
  ARequest.State := osDone;

  // Fire success/error event in main thread
  if succeeded then
    Self.FInvokeKind := iekSuccess
  else
  begin
    // If there is no HTTP status and not connected, classify as disconnection
    if (ARequest.Status = 0) and (not ARequest.Connected) then
      Self.FInvokeKind := iekDisconnected
    else
      Self.FInvokeKind := iekError;
  end;
  Self.FInvokeRequest := ARequest;
  Self.Synchronize(@DoInvokeEvents);
  Self.FOwner.FIsConnectionOpen := False;

  // Prepare and invoke the user callback in main thread
  ARequest.Response.Position := 0;
  Self.FInvokeRequest := ARequest;
  Self.FInvokeCallbackProc := ARequest.Callback;
  Self.Synchronize(@DoInvokeCallback);

  // After callback returns, cleanup
  Self.FOwner.RemoveRequestName(ARequest.OperationName);
  FreeAndNil(ARequest);
end;

{ TAsyncHTTP }

constructor TAsyncHTTP.Create;
begin
  inherited Create;
  Self.FConnectTimeout := 0;
  Self.FRetryCount := 1; // Simple automatic retry on failure
  Self.FIOTimeout := 0;
  Self.FTerminated := False;
  Self.FKeepConnection := False;
  Self.FQueue := TQueueHttpRequest.Create;
  Self.FQueueLock := TCriticalSection.Create;
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

procedure TAsyncHTTP.EnqueueRequest(const ARequest: THttpRequest);
begin
  // Map operation name to request if provided
  if ARequest.OperationName <> '' then
    Self.AddRequestName(ARequest);

  Self.FQueueLock.Acquire;
  try
    ARequest.State := osQueued;
    Self.FQueue.Push(ARequest);
    // Do not free ARequest here; ownership passes to the queue/worker
  finally
    Self.FQueueLock.Release;
  end;
  // Wake worker
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
    Self.FNamedRequestsDict[ARequest.OperationName] := ARequest;
  finally
    Self.FNamedRequestsLock.Release;
  end;
end;

procedure TAsyncHTTP.RemoveRequestName(const OperationName: string);
begin
  if OperationName = '' then Exit;
  Self.FNamedRequestsLock.Acquire;
  try
    Self.FNamedRequestsDict.delete(OperationName);
  finally
    Self.FNamedRequestsLock.Release;
  end;
end;

procedure TAsyncHTTP.Get(url: string; callback: THttpRequestCallbackFunction; headers: string; operationName: string; userObject: TObject; userString: string);
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
  Self.EnqueueRequest(req);
end;

procedure TAsyncHTTP.Post(url: string; data: string; callback: THttpRequestCallbackFunction; headers: string; operationName: string; userObject: TObject; userString: string);
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
  Self.EnqueueRequest(req);
end;

procedure TAsyncHTTP.HttpMethod(method: string; url: string; callback: THttpRequestCallbackFunction; headers: string; data: string; operationName: string; userObject: TObject; userString: string);
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
  Self.EnqueueRequest(req);
end;

procedure TAsyncHTTP.Terminate;
var
  req: THttpRequest;
begin
  if Self.FTerminated then Exit;
  Self.FTerminated := True;
  
  // Abort active client/socket ASAP
  if Assigned(Self.FCurrentClientLock) then
  begin
    Self.FCurrentClientLock.Acquire;
    try
      if Assigned(Self.FCurrentClient) then
        Self.FCurrentClient.AbortRequest;
    finally
      Self.FCurrentClientLock.Release;
    end;
  end;

  if Assigned(Self.FWorker) then
  begin
    Self.FWorker.Terminate;
    if Assigned(Self.FQueueEvent) then
      Self.FQueueEvent.SetEvent;
    Self.FWorker.WaitFor;
    FreeAndNil(Self.FWorker);
  end;

  Self.FQueueLock.Acquire;
  try
    while (Self.FQueue.Size() > 0) do
    begin
      req := Self.FQueue.Front();
      Self.FQueue.Pop();
      if req.State = osQueued then
        FreeAndNil(req);
    end;
  finally
    Self.FQueueLock.Release;
  end;

  FreeAndNil(Self.FQueueEvent);
  FreeAndNil(Self.FQueueLock);
  FreeAndNil(Self.FNamedRequestsLock);
  FreeAndNil(Self.FNamedRequestsDict);
  FreeAndNil(Self.FQueue);
  FreeAndNil(Self.FCurrentClientLock);
end;

procedure TAsyncHTTP.AbortActiveConnection;
begin
  // Abort active client/socket if present
  if Assigned(Self.FCurrentClientLock) then
  begin
    Self.FCurrentClientLock.Acquire;
    try
      if Assigned(Self.FCurrentClient) then
        Self.FCurrentClient.AbortRequest;
    finally
      Self.FCurrentClientLock.Release;
    end;
  end;
end;

function TAsyncHTTP.RequestInQueue(const OperationName: string): Boolean;
begin
  Result := GetRequestByName(OperationName) <> nil;
end;

end.


