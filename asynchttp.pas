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
    procedure HttpRequest();
  end;

  { TThreadHTTP }

  THttpCallbackEvent = procedure (Query: THttpQuery) of object;

  TQueueHttpQuery = specialize TQueue<THttpQuery>;

  THttpQueryState = (httpLoadStart, httpLoad, httpError);

  THttpQueryForThreadHTTP = class(THttpQuery)
    CallState: THttpQueryState;
    Callback: THttpCallbackEvent;
  end;

  TThreadHTTP = class(TThread)
  protected
    QueryList: TQueueHttpQuery;
    CritQueryList: TCriticalSection;
    EventWaitWork: TEventObject;

    procedure OnLoad(Query: THttpQueryBase);
    procedure OnLoadStart(Query: THttpQueryBase);
    procedure OnError(Query: THttpQueryBase);

    // All 'protected' call in this thread. All 'public' procedure call from other threads.
    procedure Execute; override;
  public
    procedure Get(const URL: string; Callback: THttpCallbackEvent;
      InWorkFlagPtr: PBoolean);
    procedure Terminate;

  end;

implementation

uses
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

{ THttpQuery }

procedure THttpQuery.HttpRequest();
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    if LoadStart<>nil then
      LoadStart(self);
    Connected := HTTP.HTTPMethod(Method, Url);
    Status := HTTP.ResultCode;
    if Connected then
    begin
      //HTTP.Sock.OnStatus:=...; - OnProgress
      if Response=nil then
        Response := TMemoryStream.Create();
      Response.LoadFromStream(HTTP.Document);
      if Load<>nil then
        Load(self);
    end else
      if Error<>nil then
        Error(self);
  finally
    HTTP.Free;
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
end;

destructor THttpQueryBase.Destroy;
begin
  if Response<>nil then
    Response.Free();
  inherited Destroy;
end;

{ TThreadHTTP }

procedure TThreadHTTP.OnLoad(Query: THttpQueryBase);
var q: THttpQueryForThreadHTTP absolute Query;
begin
  //q := THttpQueryForThreadHTTP(Query); // use 'absolute' keyword.
  q.CallState:=httpLoad;
  Application.QueueAsyncCall(TDataEvent(q.Callback), IntPtr(q));
end;

procedure TThreadHTTP.OnLoadStart(Query: THttpQueryBase);
var q: THttpQueryForThreadHTTP absolute Query;
begin
  q.CallState:=httpLoadStart;
  Application.QueueAsyncCall(TDataEvent(q.Callback), IntPtr(q));
end;

procedure TThreadHTTP.OnError(Query: THttpQueryBase);
var q: THttpQueryForThreadHTTP absolute Query;
begin
  q.CallState:=httpError;
  Application.QueueAsyncCall(TDataEvent(q.Callback), IntPtr(q));
end;

procedure TThreadHTTP.Execute;
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
        FreeAndNil(q);
      end else
        CritQueryList.Leave();
      EventWaitWork.WaitFor(INFINITE);
    end;
  finally
    FreeAndNil(CritQueryList);
    FreeAndNil(QueryList);
    FreeAndNil(EventWaitWork);
  end;
end;

procedure TThreadHTTP.Get(const URL: string; Callback: THttpCallbackEvent;
  InWorkFlagPtr: PBoolean);
var q: THttpQueryForThreadHTTP;
begin
  q := THttpQueryForThreadHTTP.Create();
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

procedure TThreadHTTP.Terminate;
begin
  inherited;
  // wake up and get out!
  EventWaitWork.SetEvent();
end;

end.

