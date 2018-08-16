unit AsyncHttp;
//todo: non lock read from multiple open socket

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
    Headers: string;
    // data for 'post' method
    Data: string;
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
  THttpQueryState = (httpLoading=3, httpDone=4, httpError=5);

  THttpQuery = class(THttpQueryBase)
  public
    {
      Opened – запрос начат.
      progress – браузер получил очередной пакет данных, можно прочитать текущие полученные данные в responseText.
      abort – запрос был отменён вызовом xhr.abort().
      error – произошла ошибка.
      load – запрос был успешно (без ошибок) завершён.
      timeout – запрос был прекращён по таймауту.
      loadend – запрос был завершён (успешно или неуспешно)
    }
    Opened: THttpQueryEvent;
    Done: THttpQueryEvent;
    Error: THttpQueryEvent;
    ReadyState: THttpQueryState;
    procedure HttpRequest();
    procedure HttpRequestEnded(); virtual;
  end;

  { TAsyncHTTP }

  THttpCallbackEvent = procedure (Query: THttpQuery) of object;

  TQueueHttpQuery = specialize TQueue<THttpQuery>;

  { THttpQueryForThreadHTTP }

  THttpEvent = procedure (Query: THttpQueryBase) of object;

  THttpQueryForThreadHTTP = class(THttpQuery)
    Callback: THttpCallbackEvent;
    procedure HttpRequestEnded(); override;
    procedure SelfDestroy(DataPtr: IntPtr);
  end;

  TAsyncHTTP = class(TThread)
  protected
    QueryList: TQueueHttpQuery;
    CritQueryList: TCriticalSection;
    EventWaitWork: TEventObject;

    procedure Execute; override;

    procedure DoLoadDone(Query: THttpQueryBase);
    procedure DoOpened(Query: THttpQueryBase);
    procedure DoError(Query: THttpQueryBase);

  public
    OnOpened: THttpEvent;
    OnLoadDone: THttpEvent;
    OnError: THttpEvent;

    // call from other threads.
    // callback call in 'main' thread (not this thread!).
    procedure HttpMethod(Method: string; const URL: string; Callback: THttpCallbackEvent; AddHeaders: string = '';
      const Data: string = ''; InWorkFlagPtr: PBoolean = nil);

    procedure Get(const URL: string; Callback: THttpCallbackEvent; AddHeaders: string = '';
      InWorkFlagPtr: PBoolean = nil);

    procedure Post(const URL: string; const Data: string;
      Callback: THttpCallbackEvent; AddHeaders: string = '';
      InWorkFlagPtr: PBoolean = nil);

    // call from other threads.
    procedure Terminate;

    destructor Destroy; override;
  end;

implementation

uses
  Forms, Controls, Graphics, Dialogs, StdCtrls, synautil,
  ExtCtrls;

{ THttpQueryForThreadHTTP }

procedure THttpQueryForThreadHTTP.HttpRequestEnded();
begin
  Application.QueueAsyncCall(@SelfDestroy, IntPtr(Self));
end;

procedure THttpQueryForThreadHTTP.SelfDestroy(DataPtr: IntPtr);
begin
  //todo: всеравно ловлю AV - в сложных/неудачных случаях
  //  нужно финальный вызов callback делать через "прокси" процедуру,
  //  которая по завершении гарантированно будет разрушать обьект.
  //  потомучто просто скидывать вызов в "стек асинхронных вызовов" это неправильно,
  //  формально говоря никто не обящает сохранения последовательности выполнения.
  // free item
  THttpQuery(DataPtr).Free();
end;

{ THttpQuery }

procedure THttpQuery.HttpRequestEnded();
begin

end;

procedure THttpQuery.HttpRequest();
begin
  Session := THTTPSend.Create;
  try
    if Opened<>nil then
      Opened(self);
    if Headers<>'' then
      Session.Headers.AddText(Headers);
    Headers:='';
    if Data<>'' then
      WriteStrToStream(Session.Document, Data);
    Data := '';

    Connected := Session.HTTPMethod(Method, Url);
    Status := Session.ResultCode;
    if Connected then
    begin
      //HTTP.Sock.OnStatus:=...; - OnProgress
      if Response=nil then
        Response := TMemoryStream.Create();
      if Session.Document.Size > 0 then
      begin
        Response.LoadFromStream(Session.Document);
        Response.Position:=0;
      end;
      ReadyState:=httpDone;
      if Done<>nil then
        Done(self);
    end else
    begin
      ReadyState:=httpError;
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
  Response := TMemoryStream.Create();
end;

destructor THttpQueryBase.Destroy;
begin
  if Response<>nil then
    Response.Free();
  inherited Destroy;
end;

{ TAsyncHTTP }

procedure TAsyncHTTP.DoOpened(Query: THttpQueryBase);
begin
  if OnOpened<>nil then
    OnOpened(Query);
end;

procedure TAsyncHTTP.DoLoadDone(Query: THttpQueryBase);
var q: THttpQueryForThreadHTTP absolute Query;
begin
  //off: q := THttpQueryForThreadHTTP(Query); - use 'absolute' keyword.

  if q.Callback <> nil then
    // 'put' call to main thread. Main thread must have "Form" or just call "CheckSynchronize"
    Application.QueueAsyncCall(TDataEvent(q.Callback), IntPtr(q));
end;

procedure TAsyncHTTP.DoError(Query: THttpQueryBase);
var q: THttpQueryForThreadHTTP absolute Query;
begin
  if q.Callback <> nil then
    Application.QueueAsyncCall(TDataEvent(q.Callback), IntPtr(q));
end;

procedure TAsyncHTTP.HttpMethod(Method: string; const URL: string;
  Callback: THttpCallbackEvent; AddHeaders: string; const Data: string;
  InWorkFlagPtr: PBoolean);
var q: THttpQueryForThreadHTTP;
begin
  if not Terminated then
  begin
    if InWorkFlagPtr<>nil then
      InWorkFlagPtr^:=true;
    q := THttpQueryForThreadHTTP.Create();
    q.Method:=Method;
    q.Url:=URL;
    q.Data:=Data;
    q.Headers:=AddHeaders;
    if InWorkFlagPtr<>nil then
      q.InWorkFlagPtr:=InWorkFlagPtr;
    q.Done:=@DoLoadDone;
    q.Error:=@DoError;
    q.Opened:=@DoOpened;
    q.Callback:=Callback;

    CritQueryList.Enter();
    QueryList.Push(q);
    CritQueryList.Leave();
    EventWaitWork.SetEvent();
  end;
end;

procedure TAsyncHTTP.Execute;
var
  q: THttpQuery;
begin
  // program init:
  CritQueryList := TCriticalSection.Create();
  QueryList:= TQueueHttpQuery.Create();
  EventWaitWork:= TEventObject.Create(nil, False, False, '');
  try
    // main loop
    while not Terminated do
    begin
      CritQueryList.Enter(); // <<

      if QueryList.Size() > 0 then
      begin
        q := QueryList.Front();
        QueryList.Pop();
        CritQueryList.Leave(); // >>
        // network stun!
        q.HttpRequest();
        //note: About 'q'. Can't run 'free' here - async callback will happen later.
        //note: About 'q'. Item not needed destroy - he used 'SelfDestroy' procedure.
      end else
        CritQueryList.Leave(); // >>

      EventWaitWork.WaitFor(INFINITE);
    end;
  finally
    // program end:

    // clear QueryList
    CritQueryList.Enter(); // <<
    while QueryList.Size() > 0 do
    begin
      q := QueryList.Front();
      QueryList.Pop();
      FreeAndNil(q);
    end;
    CritQueryList.Leave(); // >>

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
  AddHeaders: string; InWorkFlagPtr: PBoolean);
begin
  HttpMethod('GET', Url, Callback, AddHeaders, '', InWorkFlagPtr);
end;

procedure TAsyncHTTP.Post(const URL: string; const Data: string; Callback: THttpCallbackEvent;
  AddHeaders: string; InWorkFlagPtr: PBoolean);
begin
  HttpMethod('POST', Url, Callback, AddHeaders, Data, InWorkFlagPtr);
end;

procedure TAsyncHTTP.Terminate;
begin
  inherited; //note: <- FTerminated := True;
  // wake up and get out!
  if EventWaitWork <> nil then
    EventWaitWork.SetEvent();
end;

end.

