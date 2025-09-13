unit AsyncHTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fphttpclient, ghashmap, HashMapStr, gqueue;

type
  // Operation status for CheckOperation
  TOperationState = (
    osNotFound,   // Operation not found (never existed or already cleaned up)
    osQueued,     // Operation enqueued and waiting in queue
    osProcessing, // Operation picked up by worker and being processed
    osDone        // Operation finished, callback is pending/not yet cleaned up
  );

  // Internal kind of event to invoke in main thread
  TInvokeEventKind = (
    iekNone,
    iekOpened,
    iekSuccess,
    iekError
  );

  // Forward declaration
  TAsyncHTTP = class;
  THttpRequest = class;

  // Callback signature
  CallbackFunction = procedure(Request: THttpRequest) of object;

  // Request object passed to user callback
  THttpRequest = class
  public
    // HTTP response status code (0 if not available due to error)
    Status: Integer;
    // Response stream containing server data (position reset to 0 before callback)
    Response: TMemoryStream;
    // True if connection and request succeeded, false otherwise
    Connected: Boolean;
    // Request URL for reference
    Url: string;
    // HTTP method (GET/POST/...) for reference
    Method: string;
    // Operation state for this request lifecycle
    State: TOperationState;
    // Raw HTTP headers to be applied before sending
    HeadersRaw: string;
    // Optional request body for POST/other methods
    Data: string;
    // User callback to be invoked in main thread
    Callback: CallbackFunction;
    // Optional operation name for external tracking
    OperationName: string;
    constructor Create;
    destructor Destroy; override;
  end;

  // Simple event signature
  TEventHandler = procedure(Sender: TObject) of object;

  // Dictionary mapping operation name to request instance
  TDictHttpRequest = specialize THashMap<string, THttpRequest, THashFuncString>;

  // FIFO queue for requests
  TQueueHttpRequest = specialize TQueue<THttpRequest>;

  // Worker thread processing queued HTTP requests

  { TRequestWorkerThread }

  TRequestWorkerThread = class(TThread)
  private
    FOwner: TAsyncHTTP;
    FInvokeCallbackRequest: THttpRequest;
    FInvokeCallbackProc: CallbackFunction;
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
    FOnOpened: TEventHandler;
    FOnLoadDone: TEventHandler;
    FOnError: TEventHandler;
    FRetryCount: Integer;
    FIOTimeout: Integer;
    FTerminated: Boolean;
    FKeepConnection: Boolean;
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
      callback: CallbackFunction;
      headers: string = '';
      operationName: string = '');

    procedure Post(url: string;
      data: string; callback:
      CallbackFunction;
      headers: string = '';
      operationName: string = '');

    procedure HttpMethod(
      method: string;
      url: string;
      callback: CallbackFunction;
      headers: string = '';
      data: string = '';
      operationName: string = '');

    // Graceful shutdown
    procedure Terminate;

    // Check operation status by name
    function RequestInQueue(const OperationName: string): Boolean;

    // Properties
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property IOTimeout: Integer read FIOTimeout write FIOTimeout;
    property RetryCount: Integer read FRetryCount write FRetryCount;
    // When true, reuse one HTTP client (keep-alive) within the worker thread
    property KeepConnection: Boolean read FKeepConnection write FKeepConnection;
    property OnOpened: TEventHandler read FOnOpened write FOnOpened;
    property OnLoadDone: TEventHandler read FOnLoadDone write FOnLoadDone;
    property OnError: TEventHandler read FOnError write FOnError;
  end;

implementation

{ TQuietHTTPClient }

function TQuietHTTPClient.CheckResponseCode(ACode: Integer; const AllowedResponseCodes: array of Integer): Boolean;
begin
  // Always treat any HTTP response code as acceptable to prevent exceptions.
  Result := True;
end;

{ TAsyncHTTP - virtual factory/configuration }

function TAsyncHTTP.CreateHttpClient: TFPHTTPClient;
begin
  // Create client that does not raise on non-2xx/3xx codes
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

{ THttpRequest }

constructor THttpRequest.Create;
begin
  inherited Create;
  Self.Status := 0;
  Self.Response := nil;
  Self.Connected := False;
  Self.State := osNotFound;
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
  Self.FInvokeCallbackRequest := nil;
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
        Self.FOwner.FOnOpened(Self.FOwner);
    iekSuccess:
      if Assigned(Self.FOwner.FOnLoadDone) then
        Self.FOwner.FOnLoadDone(Self.FOwner);
    iekError:
      if Assigned(Self.FOwner.FOnError) then
        Self.FOwner.FOnError(Self.FOwner);
  else
    ;
  end;
end;

procedure TRequestWorkerThread.DoInvokeCallback;
begin
  // Execute callback in the main/UI thread
  if Assigned(Self.FInvokeCallbackProc) and Assigned(Self.FInvokeCallbackRequest) then
    Self.FInvokeCallbackProc(Self.FInvokeCallbackRequest);
end;

procedure TRequestWorkerThread.Execute;
var
  req: THttpRequest;
  sharedClient: TFPHTTPClient;
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
          Self.FOwner.FQueue.Pop();
        end;
      finally
        Self.FOwner.FQueueLock.Release;
      end;

      if req = nil then Break;

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
  Self.Synchronize(@DoInvokeEvents);

  succeeded := False;
  ARequest.Method := UpperCase(ARequest.Method);
  ARequest.State := osProcessing;

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

    for attempt := 0 to Self.FOwner.FRetryCount do
    begin
      if ARequest.Response = nil then
        ARequest.Response := TMemoryStream.Create
      else
        ARequest.Response.Size := 0;
      ARequest.Response.Position := 0;

      try
        if ARequest.Method = 'GET' then
          Client.Get(ARequest.Url, ARequest.Response)
        else if ARequest.Method = 'POST' then
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
            Client.HTTPMethod(ARequest.Method, ARequest.Url, ARequest.Response, []);
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
        // On any exception, retry if attempts remain
        on E: Exception do
        begin
          ARequest.Connected := False;
          ARequest.Status := 0;
        end;
      end;
    end;
  finally
    if clientNeedFree then
      Client.Free;
  end;

  // Mark as done before invoking callback; after callback we will remove operation
  ARequest.State := osDone;

  // Fire success/error event in main thread
  if succeeded then
    Self.FInvokeKind := iekSuccess
  else
    Self.FInvokeKind := iekError;
  Self.Synchronize(@DoInvokeEvents);

  // Prepare and invoke the user callback in main thread
  ARequest.Response.Position := 0;
  Self.FInvokeCallbackRequest := ARequest;
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

procedure TAsyncHTTP.Get(url: string; callback: CallbackFunction; headers: string; operationName: string);
var
  req: THttpRequest;
begin
  req := THttpRequest.Create;
  req.Method := 'GET';
  req.Url := url;
  req.HeadersRaw := headers;
  req.Data := '';
  req.Callback := callback;
  req.OperationName := operationName;
  Self.EnqueueRequest(req);
end;

procedure TAsyncHTTP.Post(url: string; data: string; callback: CallbackFunction; headers: string; operationName: string);
var
  req: THttpRequest;
begin
  req := THttpRequest.Create;
  req.Method := 'POST';
  req.Url := url;
  req.HeadersRaw := headers;
  req.Data := data;
  req.Callback := callback;
  req.OperationName := operationName;
  Self.EnqueueRequest(req);
end;

procedure TAsyncHTTP.HttpMethod(method: string; url: string; callback: CallbackFunction; headers: string; data: string; operationName: string);
var
  req: THttpRequest;
begin
  req := THttpRequest.Create;
  req.Method := UpperCase(Trim(method));
  if req.Method = '' then req.Method := 'GET';
  req.Url := url;
  req.HeadersRaw := headers;
  req.Data := data;
  req.Callback := callback;
  req.OperationName := operationName;
  Self.EnqueueRequest(req);
end;

procedure TAsyncHTTP.Terminate;
var
  req: THttpRequest;
begin
  if Self.FTerminated then Exit;
  Self.FTerminated := True;
  
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
end;

function TAsyncHTTP.RequestInQueue(const OperationName: string): Boolean;
begin
  Result := GetRequestByName(OperationName) <> nil;
end;

end.


