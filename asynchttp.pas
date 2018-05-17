unit AsyncHttp;

{$mode objfpc}{$H+}

interface

uses
  httpsend,
  syncobjs,
  gqueue,
  Classes, SysUtils;

type
  { THttpQueryBase }

  THttpQueryBase = class
  public
    Session: THTTPSend;
    Method: string;
    Url: string;
    //user, password
    Status: integer;
    // readed data
    Response: TMemoryStream;
    Connected: boolean;
    InWorkFlagPtr: PBoolean;
    procedure SetRequestHeader(name: string; value: string);

    constructor Create;
    destructor Destroy; override;
  end;

  THttpQueryEvent = procedure (Query: THttpQueryBase) of object;

  {
    UNSENT = 0; // начальное состояние
    OPENED = 1; // вызван open
    HEADERS_RECEIVED = 2; // получены заголовки
    LOADING = 3; // загружается тело (получен очередной пакет данных)
    DONE = 4; // запрос завершён
  }
  THttpQueryState = (httpLoadStart=1, httpLoad=2, httpError=13);

  THttpQuery = class(THttpQueryBase)
  public
    {
      loadstart – запрос начат.
      progress – браузер получил очередной пакет данных, можно прочитать текущие полученные данные в responseText.
      abort – запрос был отменён вызовом xhr.abort().
      error – произошла ошибка.
      load – запрос был успешно (без ошибок) завершён.
      timeout – запрос был прекращён по таймауту.
      loadend – запрос был завершён (успешно или неуспешно)
    }
    LoadStart: THttpQueryEvent;
    Load: THttpQueryEvent;
    Error: THttpQueryEvent;
    CallState: THttpQueryState;
    procedure HttpRequestEnded(); virtual;
    procedure HttpRequest();
  end;

  { TAsyncHTTP }

  THttpCallbackEvent = procedure (Query: THttpQuery) of object;

  TQueueHttpQuery = specialize TQueue<THttpQuery>;

  { THttpQueryForThreadHTTP }

  THttpQueryForThreadHTTP = class(THttpQuery)
    Callback: THttpCallbackEvent;
    procedure HttpRequestEnded(); override;
    procedure SelfDestroy(Data: IntPtr);
  end;

  TAsyncHTTP = class(TThread)
  protected
    QueryList: TQueueHttpQuery;
    CritQueryList: TCriticalSection;
    EventWaitWork: TEventObject;

    procedure OnLoad(Query: THttpQueryBase);
    procedure OnLoadStart(Query: THttpQueryBase);
    procedure OnError(Query: THttpQueryBase);

    // All 'protected' call in this thread. All 'public' procedure call from other threads.
    procedure Execute; override;

    //procedure AsyncProcProxy(p: IntPtr);

  public
    procedure Get(const URL: string; Callback: THttpCallbackEvent;
      InWorkFlagPtr: PBoolean = nil);
    procedure Terminate;

    destructor Destroy; override;
  end;

implementation

uses
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

{ THttpQueryForThreadHTTP }

procedure THttpQueryForThreadHTTP.HttpRequestEnded();
begin
  Application.QueueAsyncCall(@SelfDestroy, IntPtr(Self));
end;

procedure THttpQueryForThreadHTTP.SelfDestroy(Data: IntPtr);
begin
  // free item
  THttpQuery(Data).Free();
end;

{ THttpQuery }

procedure THttpQuery.HttpRequestEnded();
begin

end;

procedure THttpQuery.HttpRequest();
begin
  Session := THTTPSend.Create;
  try
    CallState:=httpLoadStart;
    if LoadStart<>nil then
      LoadStart(self);
    Connected := Session.HTTPMethod(Method, Url);
    Status := Session.ResultCode;
    if Connected then
    begin
      //HTTP.Sock.OnStatus:=...; - OnProgress
      //if Response=nil then Response := TMemoryStream.Create();
      if Session.Document.Size > 0 then
      begin
        Response.LoadFromStream(Session.Document);
        Response.Position:=0;
      end;
      CallState:=httpLoad;
      if Load<>nil then
        Load(self);
    end else
    begin
      CallState:=httpError;
      if Error<>nil then
        Error(self);
    end;
  finally
    FreeAndNil(Session);
    if InWorkFlagPtr<>nil then
      InWorkFlagPtr^:=false;
    HttpRequestEnded();
  end;
end;

{ THttpQueryBase }

procedure THttpQueryBase.SetRequestHeader(name: string; value: string);
begin
  Session.Headers.Add(name+': '+value);
end;

constructor THttpQueryBase.Create;
begin
  //
  Response := TMemoryStream.Create();
end;

destructor THttpQueryBase.Destroy;
begin
  if Response<>nil then
    Response.Free();
  inherited Destroy;
end;

{ TAsyncHTTP }

procedure TAsyncHTTP.OnLoad(Query: THttpQueryBase);
var q: THttpQueryForThreadHTTP absolute Query;
begin
  //q := THttpQueryForThreadHTTP(Query); // use 'absolute' keyword.
  Application.QueueAsyncCall(TDataEvent(q.Callback), IntPtr(q));
end;

procedure TAsyncHTTP.OnLoadStart(Query: THttpQueryBase);
var q: THttpQueryForThreadHTTP absolute Query;
begin
  Application.QueueAsyncCall(TDataEvent(q.Callback), IntPtr(q));
end;

procedure TAsyncHTTP.OnError(Query: THttpQueryBase);
var q: THttpQueryForThreadHTTP absolute Query;
begin
  Application.QueueAsyncCall(TDataEvent(q.Callback), IntPtr(q));
end;

procedure TAsyncHTTP.Execute;
var
  q: THttpQuery;
begin
  CritQueryList := TCriticalSection.Create();
  QueryList:= TQueueHttpQuery.Create();
  EventWaitWork:= TEventObject.Create(nil, False, False, '');
  try
    while not Terminated do
    begin
      CritQueryList.Enter();
      if QueryList.Size() > 0 then
      begin
        q := QueryList.Front();
        QueryList.Pop();
        CritQueryList.Leave();
        // network stun!
        q.HttpRequest();
        //note: About 'q'. Can't run 'free' here - async callback will happen later.
        //note: About 'q'. Item not needed destroy - he used 'SelfDestroy' procedure.
      end else
        CritQueryList.Leave();
      EventWaitWork.WaitFor(INFINITE);
    end;
  finally
    //todo: clear QueryList!
    FreeAndNil(CritQueryList);
    FreeAndNil(QueryList);
    FreeAndNil(EventWaitWork);
  end;
end;

destructor TAsyncHTTP.Destroy;
begin
  if not Terminated then
    Terminate();
  inherited Destroy;
end;

procedure TAsyncHTTP.Get(const URL: string; Callback: THttpCallbackEvent;
  InWorkFlagPtr: PBoolean);
var q: THttpQueryForThreadHTTP;
begin
  if not Terminated then
  begin
    if InWorkFlagPtr<>nil then
      InWorkFlagPtr^:=true;
    q := THttpQueryForThreadHTTP.Create();
    q.Method:='GET';
    q.Url:=URL;
    if InWorkFlagPtr<>nil then
      q.InWorkFlagPtr:=InWorkFlagPtr;
    q.Load:=@OnLoad;
    q.Error:=@OnError;
    q.LoadStart:=@OnLoadStart;
    q.Callback:=Callback;

    CritQueryList.Enter();
    QueryList.Push(q);
    CritQueryList.Leave();
    EventWaitWork.SetEvent();
  end;
end;

procedure TAsyncHTTP.Terminate;
begin
  inherited;
  // wake up and get out!
  EventWaitWork.SetEvent();
end;

end.

