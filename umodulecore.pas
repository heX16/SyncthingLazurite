unit uModuleCore;

{$mode objfpc}{$H+}

interface

uses
  Dialogs,
  AsyncHttp,
  fpjson,
  XMLRead, DOM,
  ghashmap,
  HashMapStr,
  LazFileUtils, LazUTF8,
  Classes, SysUtils, UTF8Process, ExtCtrls, ActnList, Forms;


type
  { TDevInfo }
  TDevId = ansistring;
  TFolderId = ansistring;

  // device info 'record'
  TDevInfo = class
    Json: TJSONObject;
    Id: string;
    Name: string;
    Address: string;
    Connected: boolean;
    Paused: boolean;
    LastSeen: TDateTime;

    constructor Create(SetJson: TJSONObject);
    destructor Destroy(); override;
    procedure Update(NewJson: TJSONObject); virtual;
  end;


  { TFolderInfo }

  // folders info 'record'
  TFolderInfo = class
    Json: TJSONObject;
    Name: string;
    Id: string;
    Path: string;

    constructor Create(SetJson: TJSONObject);
    destructor Destroy(); override;
    procedure Update(NewJson: TJSONObject); virtual;
    function DirectoryExists(): Boolean;
  end;

  TMapDevInfo = specialize THashMap<TDevId, TDevInfo, THashFuncString>;
  TMapFolderInfo = specialize THashMap<TFolderId, TFolderInfo, THashFuncString>;

type

  TAddConsoleLine = procedure (Line: UTF8String) of object;

  { TCore }

  //todo: extract real core code to 'model'(or 'control') and 'utils'
  TCore = class(TDataModule)
    actExit: TAction;
    actInit: TAction;
    actReloadConfig: TAction;
    actPause: TAction;
    actRunSupportProc: TAction;
    actTerminate: TAction;
    actRestart: TAction;
    actStop: TAction;
    actStart: TAction;
    ActionList: TActionList;
    ProcessSyncthing: TProcessUTF8;
    ProcessSupport: TProcessUTF8;
    TimerCheckState: TTimer;
    TimerAfterStartCheck: TTimer;
    TimerPause: TTimer;
    TimerInit: TTimer;
    TimerPing: TTimer;
    TimerReadStdOutput: TTimer;
    TimerStartOnStart: TTimer;
    procedure actInitExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actReloadConfigExecute(Sender: TObject);
    procedure actRestartExecute(Sender: TObject);
    procedure actRunSupportProcExecute(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actTerminateExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);

    procedure DataModuleDestroy(Sender: TObject);
    procedure TimerAfterStartCheckTimer(Sender: TObject);
    procedure TimerCheckStateTimer(Sender: TObject);

    procedure TimerInitTimer(Sender: TObject);
    procedure TimerPauseTimer(Sender: TObject);
    procedure TimerStartOnStartTimer(Sender: TObject);
    procedure TimerPingTimer(Sender: TObject);
    procedure TimerReadStdOutputTimer(Sender: TObject);
  private
    OutputChankStr: UTF8String;
  public

    // Flag that core is inited
    Inited: boolean;

    // State (FSM)
    //TODO: WIP!
    State: (stUnknown, stBrokenAndDisabled, stStopped, stLaunching, stWork, stStopping, stInstantStopped);

    // flag - syncthing is work
    IsOnline: boolean;
    // flag - checked 'syncthing is work'
    OnlineTested: boolean;

    Terminated: boolean;
    aiohttp: TAsyncHTTP;

    APIKey: string;
    SyncthigExecPath: UTF8String;

    // data dir. / config dir.
    SyncthigHome: UTF8String;

    SyncthigParam: UTF8String;
    SyncthigServer: UTF8String;
    SyncthigHost: UTF8String;
    SyncthigPort: integer;

    httpPingInProc: boolean;

    EventsLastId: int64;

    MapDevInfo: TMapDevInfo;
    MapFolderInfo: TMapFolderInfo;
    ListDevInfo: TStringList;
    ListFolderInfo: TStringList;

    procedure Init(); virtual;
    procedure Done(); virtual;
    procedure EventOnline(); virtual;
    procedure EventOffline(); virtual;

    function ListDev_GetText(NodeIndex: Cardinal): String;

    function GetSyncthigExecPath(): UTF8String; virtual;
    function GetSyncthigHomePath(): UTF8String; virtual;
    function ReadAPIKeyFromCfg(): string; virtual;
    procedure LoadDevices(Json: TJSONData);
    procedure LoadFolders(Json: TJSONData);
    procedure FillSyncthingExecPath();
    procedure FillSupportExecPath();

    procedure Start();
    procedure Stop();

    procedure ReadStdOutput(Proc: TProcessUTF8; AddProc: TAddConsoleLine; var TextChank: UTF8String);
    procedure AddStringToConsole(Str: UTF8String);

    function SendJSON(const RESTPath: string; const DataForSend: TStrings = nil): Boolean;

    // call from other thread!
    procedure aiohttpAddHeader(Request: THttpRequest; Sender: TObject);

    // read devices and folders config
    procedure httpReadConfig(Request: THttpRequest);

    procedure httpUpdateConnections(Request: THttpRequest);

    procedure httpPing(Request: THttpRequest);
  end;

var
  Core: TCore;

function HttpRequestToJson(Request: THttpRequest; out Json: TJSONData): boolean;

function JsonStrToDateTime(Str: AnsiString; out dt: TDateTime): boolean;

function IsLocalIP(IP: string): boolean;

implementation

uses
  CRC,
  LCLTranslator, // i18n
  uFormOptions,
  uFormMain,
  httpsend, {Synacode,}
  synautil,
  Graphics,
  LConvEncoding,
  RegExpr, // ParseJsonDateTime
  dateutils, // ParseJsonDateTime
  jsonparser,
  jsonscanner;

{$R *.lfm}

function JsonStrToDateTime(Str: AnsiString; out dt: TDateTime): boolean;
var re: TRegExpr;
begin
  //todo: move to global singlton
  re := TRegExpr.Create('(\d+)-(\d+)-(\d+)T(\d+):(\d+):(\d+).*');

  Result := false;
  try
    if re.Exec(Str) then
    begin
      Result := TryEncodeDateTime(
        StrToInt(re.Match[1]),
        StrToInt(re.Match[2]),
        StrToInt(re.Match[3]),
        StrToInt(re.Match[4]),
        StrToInt(re.Match[5]),
        StrToInt(re.Match[6]),
        0,
        dt
        );
    end;
  finally
    re.Free();
  end;
end;

function IsLocalIP(IP: string): boolean;
var
  B1, B2, B3, B4: byte;
  Parts: array[0..3] of string;
  PartCount: integer;
  i: integer;
  Part: string;
  PartValue: integer;
begin
  Result := false;
  IP := LowerCase(Trim(IP));
  
  if IP = '' then
    Exit;
  
  // Check if it looks like IPv6 (contains colon)
  if Pos(':', IP) > 0 then
  begin
    // Simple IPv6 checks

    if LeftStr(IP, 1) = '[' then
      Delete(IP, 1, 1);
    
    // Check for ::1 (localhost)
    if (IP = '::1') then
      Exit(true);
    
    // Check for fe80:: (link-local)
    if (Length(IP) >= 5) and (LeftStr(IP, 5) = 'fe80:') then
      Exit(true);
    
    // Check for ULA (fc00:: and fd00::) (unique local address)
    if (Length(IP) >= 2) and
       ((LeftStr(IP, 2) = 'fc') or (LeftStr(IP, 2) = 'fd')) then
      Exit(true);
    
    // For other IPv6 addresses, assume they are not local
    Exit(false);
  end;
  
  // Parse IPv4 manually
  PartCount := 0;
  Part := '';
  
  for i := 1 to Length(IP) do
  begin
    if IP[i] = '.' then
    begin
      if (PartCount >= 4) or (Part = '') then
        Exit(false); // Invalid format
      
      if not TryStrToInt(Part, PartValue) or (PartValue < 0) or (PartValue > 255) then
        Exit(false); // Invalid number
      
      Parts[PartCount] := Part;
      Inc(PartCount);
      Part := '';
    end
    else if IP[i] in ['0'..'9'] then
    begin
      Part := Part + IP[i];
    end
    else
    begin
      Exit(false); // Invalid character
    end;
  end;
  
  // Add the last part
  if (PartCount <> 3) or (Part = '') then
    Exit(false); // Should have exactly 4 parts
  
  if not TryStrToInt(Part, PartValue) or (PartValue < 0) or (PartValue > 255) then
    Exit(false); // Invalid number
  
  Parts[3] := Part;
  
  // Convert to bytes
  B1 := StrToInt(Parts[0]);
  B2 := StrToInt(Parts[1]);
  B3 := StrToInt(Parts[2]);
  B4 := StrToInt(Parts[3]);
  
  // Check IPv4 private/local ranges
  // 127.0.0.0/8 (localhost)
  if B1 = 127 then
    Exit(true);
  
  // 10.0.0.0/8 (private)
  if B1 = 10 then
    Exit(true);
  
  // 172.16.0.0/12 (private)
  if (B1 = 172) and (B2 >= 16) and (B2 <= 31) then
    Exit(true);
  
  // 192.168.0.0/16 (private)
  if (B1 = 192) and (B2 = 168) then
    Exit(true);
  
  // 169.254.0.0/16 (link-local)
  if (B1 = 169) and (B2 = 254) then
    Exit(true);
  
  Result := false;
end;

function HttpRequestToJson(Request: THttpRequest; out Json: TJSONData): boolean;
var
  JN: TJSONParser;
  JData: TJSONData;
begin
  Result := false;
  Json := nil;
  if Request.Connected then
  begin
    try
      if (Request.Response <> nil) and (Request.Response.Size>0) then
      begin
        JData:=nil;
        JN := nil;
        JN := TJSONParser.Create(Request.Response, [joUTF8]);
        try
          JData := JN.Parse();
        except
          on EJSONParser do JData := nil;
        end;
        if JData <> nil then
        begin
          Json := JData;
          Result := true;
        end;
      end;
    finally
      if JN<>nil then
        JN.Free();
    end;
  end;
end;

{ TFolderInfo }

constructor TFolderInfo.Create(SetJson: TJSONObject);
begin
  (*
    "id" : "config",
    "label" : "config",
    "filesystemType" : "basic",
    "path" : "D:\\Sync\\config",
    "type" : "sendreceive",
    "devices" : [
      {
        "deviceID" : "XXXXXXX-VDBWOAW-QHBNBHY-2UUEL22-S4LICVU-LA6JRMO-GBR5NRG-XXXXXXX",
        "introducedBy" : ""
      },
      {
        "deviceID" : "XXXXXXX-YAT62H7-SAN2ZID-DHSFKAV-YKYCQAQ-DMCDM5K-DC67FGP-XXXXXXX",
        "introducedBy" : ""
      }
    ],
    "rescanIntervalS" : 3600,
    "fsWatcherEnabled" : true,
    "fsWatcherDelayS" : 10,
    "ignorePerms" : false,
    "autoNormalize" : true,
    "minDiskFree" : {
      "value" : 1,
      "unit" : "%"
    },
    "versioning" : {
      "type" : "",
      "params" : {}
    },
    "copiers" : 0,
    "pullerMaxPendingKiB" : 0,
    "hashers" : 0,
    "order" : "random",
    "ignoreDelete" : false,
    "scanProgressIntervalS" : 0,
    "pullerPauseS" : 0,
    "maxConflicts" : 10,
    "disableSparseFiles" : false,
    "disableTempIndexes" : false,
    "paused" : false,
    "weakHashThresholdPct" : 25,
    "markerName" : ".stfolder",
    "copyOwnershipFromParent" : false
  *)

  inherited Create();
  Json:=nil;
  Update(SetJson);
end;

destructor TFolderInfo.Destroy();
begin
  Name:='';
  Id:='';
  if Json<>nil then
    FreeAndNil(Json);
  inherited;
end;

procedure TFolderInfo.Update(NewJson: TJSONObject);
begin
  Name:='';
  Id:='';
  if Json<>nil then
    FreeAndNil(Json);

  if NewJson<>nil then begin
    // create copy of json tree
    Json := NewJson.Clone() as TJSONObject;
    Name:=Json.Get('label', 'ERROR');
    Id:=Json.Get('id', 'ERROR');
    if Name='' then
      Name:=Id;
    Path:=Json.Get('path', '');
  end;
end;

function TFolderInfo.DirectoryExists: Boolean;
var Info: TSearchRec;
begin
  Result := DirectoryExistsUTF8(self.Path);
  if Result then
  begin
    try
      If FindFirstUTF8('*', faAnyFile, Info)=0
      then
        FindCloseUTF8(Info)
      else
        Result := false;
    except
      on EFileNotFoundException do Result := false;
      on EDirectoryNotFoundException do Result := false;
    end;
  end;
end;

{ TDevInfo }

constructor TDevInfo.Create(SetJson: TJSONObject);
begin
(*
  "deviceID" : "XXXXXXX-7W6T5GU-Q7F4UB4-XXXXXXX-2NZFCAR-TONYMDU-JRCX67A-XXXXXXX",
  "name" : "heX home PC",
  "addresses" : [
    "dynamic",
    "tcp://hex.xxx.com:123",
    "tcp://192.168.1.123:123"
  ],
  "compression" : "always",
  "certName" : "",
  "introducer" : false,
  "skipIntroductionRemovals" : false,
  "introducedBy" : "",
  "paused" : false,
  "allowedNetworks" : [
  ],
  "autoAcceptFolders" : false,
  "maxSendKbps" : 0,
  "maxRecvKbps" : 0,
  "ignoredFolders" : [
  ],
  "pendingFolders" : [
  ],
  "maxRequestKiB" : 0
*)
  Inherited Create();
  Update(SetJson);
end;

destructor TDevInfo.Destroy();
begin
  Name:='';
  Id:='';
  if Json<>nil then
    FreeAndNil(Json);
  inherited;
end;

procedure TDevInfo.Update(NewJson: TJSONObject);
begin
  Name:='';
  Id:='';
  if Json<>nil then
    FreeAndNil(Json);

  if NewJson<>nil then begin
    // create copy of json tree
    Json := NewJson.Clone() as TJSONObject;
    Name:=Json.Get('name', 'ERROR');

    Id:=Json.Get('deviceID', 'ERROR');
    Paused:=Json.Get('paused', False);
    if Name='' then
      Name:=Id;
  end;
end;

{ TCore }

function TCore.SendJSON(const RESTPath: string; const DataForSend: TStrings
  ): Boolean;
var
  HTTP: THTTPSend;
  URL: string;
  s: UTF8String;
begin
  //todo: REMOVE OLD METHOD - migrate to asyncHttp
  Result := false;
  URL := SyncthigServer + RESTPath;
  HTTP := THTTPSend.Create;
  HTTP.Headers.Add('X-API-Key: '+APIKey);
  try
    if Assigned(DataForSend) then
      for s in DataForSend do
        WriteStrToStream(HTTP.Document, s);
    Result := HTTP.HTTPMethod('POST', URL);
    //if Result then
    //  Response.LoadFromStream(HTTP.Document);
  finally
    HTTP.Free;
  end;
end;

procedure TCore.aiohttpAddHeader(Request: THttpRequest; Sender: TObject);
begin
  if Pos('X-API-Key:', Request.HeadersRaw) = 0 then
  begin
    if Request.HeadersRaw <> '' then
      Request.HeadersRaw := 'X-API-Key: ' + self.APIKey + LineEnding + Request.HeadersRaw
    else
      Request.HeadersRaw := 'X-API-Key: ' + self.APIKey;
  end;
end;

procedure TCore.httpReadConfig(Request: THttpRequest);
var j: TJSONData;
begin
  if HttpRequestToJson(Request, j) then
  try
    LoadDevices(j);
    LoadFolders(j);
    frmMain.treeDevices.RootNodeCount:=ListDevInfo.Count;
    frmMain.treeFolders.RootNodeCount:=ListFolderInfo.Count;
  finally
    FreeAndNil(j);
  end;
end;

procedure TCore.httpUpdateConnections(Request: THttpRequest);
var
  JData, j2: TJSONData;
  ij: TJSONEnum;
  d: TDevInfo;
begin
  //todo: change logic - first update MapDevInfo and after update DevicesItems
  //todo: httpUpdateConnections - move to Core!
  if HttpRequestToJson(Request, JData) then
  try
    j2 := JData.GetPath('connections');
    // enum all device
    for ij in j2 do
    begin
      // find in device list.
      if Core.MapDevInfo.GetValue(ij.Key, d) then
      begin
        // update data
        d.Connected:=ij.Value.GetPath('connected').AsBoolean;
        d.Paused:=ij.Value.GetPath('paused').AsBoolean;
        d.Address:=ij.Value.GetPath('address').AsString;
        //todo: use 'mutable' ptr
        // write to map
        Core.MapDevInfo[ij.Key] := d;
      end;
    end;
    frmMain.treeDevices.RootNodeCount:=j2.Count;
    frmMain.treeDevices.Invalidate(); //todo: <-optimize
  finally
    FreeAndNil(JData);
  end;
end;

(*
Old code:
>>>>>>>>>>>>>>>
procedure TCore.HTTPClientPingDoneInput(ASocket: TLHTTPClientSocket);
begin
  OnlineTested := true;
  if not Terminated then
  begin

    OnlineTested := true;
    if ASocket.ResponseStatus = hsOK then
    begin
      //todo: check ping result

      if not IsOnline then
      begin
        IsOnline := true;
        EventOnline();
      end;

    end else
    begin

      if IsOnline then
      begin
        IsOnline := false;
        EventOffline();
      end;

    end; // if
  end;

  //???
  aSocket.Disconnect;
end;

procedure TCore.HTTPClientPingError(const msg: string; aSocket: TLSocket);
begin
  //TODO: WIP!!! 2023...
  AddStringToConsole('err: '+msg);//DBG!

  aSocket.ConnectionStatus;
  IsOnline := false;
  OnlineTested := true;
  frmMain.shStatusCircle.Brush.Color:=clRed;
end;

function TCore.HTTPClientPingInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
  ASize: integer): integer;
begin
  Result := aSize; // tell the http buffer we read it all
end;

procedure TCore.HTTPClientPingDisconnect(aSocket: TLSocket);
begin
  AddStringToConsole('diss!!!');//DBG!
  IsOnline := false;
  OnlineTested := true;
  frmMain.shStatusCircle.Brush.Color:=clRed;
end;
<<<<<<<<<<<<<<<<<<<<
*)

procedure TCore.TimerPingTimer(Sender: TObject);
begin
  if not Terminated then begin
    if not aiohttp.RequestInQueue('system-ping') then
      aiohttp.Get(SyncthigServer+'rest/system/ping', @httpPing, '', 'system-ping');
    if not IsOnline then
      TimerPing.Interval:=1000 else
      TimerPing.Interval:=5000;
  end;
end;

procedure TCore.httpPing(Request: THttpRequest);
begin
  OnlineTested := true;
  if not Terminated then
    if Request.Connected then
    begin
      //todo: check ping result
      if Request.Status <> 200 then
      begin
        if IsOnline then
        begin
          IsOnline := false;
          EventOffline();
        end;
      end else
      begin
        if not IsOnline then
        begin
          IsOnline := true;
          EventOnline();
        end;
      end;
    end else
    begin
      IsOnline := false;
      frmMain.shStatusCircle.Brush.Color:=clRed;
    end;
end;

//todo: TEMP!!!!!!!!!
function DoFill(Node:TDOMNode): widestring;
var
  i: integer;
begin
  Result := '';
  //if not Assigned(Node) then exit;
  for i:=0 to Node.ChildNodes.Count - 1 do
  begin
    Result := Result + Node.ChildNodes[i].NodeName + ', ';
    Result := Result + ' (' + DoFill(Node.ChildNodes[i]) + '), ';
  end;
end;

function GetXml(var Node:TDOMNode; Name: WideString): boolean;
var N:TDOMNode;
begin
  Result := false;
  if Node <> nil then
  begin
    N := Node.FindNode(Name);
    if N <> nil then
    begin
      Node:=N;
      Result:=true;
    end;
  end;
end;

function TCore.ReadAPIKeyFromCfg: string;
var
  //key: string;
  filename: UTF8String;
  FDoc: TXMLDocument;
  NPtr: TDOMNode;
begin
  try
    FDoc := nil;
    filename := GetSyncthigHomePath() + '\config.xml';
    if FileExistsUTF8(filename) then
      ReadXMLFile(FDoc, UTF8ToSys(filename));

    NPtr := FDoc;
    if GetXml(NPtr, 'configuration') then
      if GetXml(NPtr, 'gui') then
        if GetXml(NPtr, 'apikey') then
          if GetXml(NPtr, '#text') then
          begin
            Result := AnsiString(NPtr.NodeValue);
          end;
  finally
    if Assigned(FDoc) then
      FreeAndNil(FDoc);
  end;

  if Result = '' then
    Result := frmOptions.edAPIKey.Text;
end;

procedure TCore.LoadDevices(Json: TJSONData);
var
  JData: TJSONData;
  ij: TJSONEnum;
  j: TJSONObject;
  id: string;
  d: TDevInfo;
begin
  JData := Json.FindPath('devices');
  ListDevInfo.Clear();
  if JData <> nil then
    for ij in JData do
    begin
      if ij.Value.InheritsFrom(TJSONObject) then
      begin
        j := ij.Value as TJSONObject;
        id := j.Get('deviceID', 'ERROR');
        if MapDevInfo.contains(id) then begin
          d := MapDevInfo.Items[id];
          d.Update(j);
        end else begin
          d := TDevInfo.Create(j);
          MapDevInfo.insert(id, d);
        end;
        ListDevInfo.Add(d.Id);
      end;
    end;
end;

procedure TCore.LoadFolders(Json: TJSONData);
var
  JData: TJSONData;
  ij: TJSONEnum;
  j: TJSONObject;
  id: string;
  d: TFolderInfo;
begin
  JData := Json.FindPath('folders');
  ListFolderInfo.Clear();
  if JData <> nil then
    for ij in JData do
    begin
      if ij.Value.InheritsFrom(TJSONObject) then
      begin
        j := ij.Value as TJSONObject;
        id := j.Get('id', 'ERROR');
        if MapFolderInfo.contains(id) then begin
          d := MapFolderInfo.Items[id];
          d.Update(j);
        end else begin
          d := TFolderInfo.Create(j);
          MapFolderInfo.insert(id, d);
        end;
        ListFolderInfo.Add(d.Id);
      end;
    end;
end;

procedure TCore.Start();
begin
  //TODO: добавить поддержку "подключения" к уже запущенному процессу (без консоли разумеется)
  // если процесс не запущен тогда запускаем его
  if not ProcessSyncthing.Running then
  begin
    FillSyncthingExecPath();
    ProcessSyncthing.Execute();
    TimerAfterStartCheck.Enabled:=True;
    if actRunSupportProc.Checked then
    begin
      FillSupportExecPath();
      ProcessSupport.Execute();
    end;
  end;
end;

procedure TCore.Stop();
begin
  //todo: WIP!!!! - чет не заработал POST метод...
  //todo: OLD:
  SendJSON('rest/system/shutdown');
  //aiohttp.Post('rest/system/shutdown', '', nil);
  if ProcessSupport.Active then
    ProcessSupport.Terminate(0);
  //ProcessSyncthing.Terminate(0);
end;

procedure TCore.ReadStdOutput(Proc: TProcessUTF8;
  AddProc: TAddConsoleLine; var TextChank: UTF8String);
var
  line, TmpStr: RawByteString;
  p, BytesRead: LongInt;
  conv_ok: boolean;
const
  LE = LineEnding; //todo: check in linux
begin
  try
    if Assigned(Proc.Output) then
    begin
      //todo: (ProcessSyncthing.Running) ...
      BytesRead:=Proc.Output.NumBytesAvailable;
      if (BytesRead > 0) then
      begin
        SetLength(TmpStr, BytesRead);
        Proc.Output.ReadBuffer(TmpStr[1], BytesRead);
        TmpStr := TextChank + TmpStr;
        line := '';
        p := Pos(LE, TmpStr);
        while p <> 0 do
        begin
          // get line
          line := Copy(TmpStr, 1, p-1);
          // cut line from text
          TmpStr := Copy(TmpStr, p+Length(LE), Length(TmpStr) - p);
          // normalize codepage from console (uses LConvEncoding)
          line := ConvertEncodingToUTF8(line, GetConsoleTextEncoding(), conv_ok);
          // send line
          AddProc(line);
          // find again
          p := Pos(LE, TmpStr);
        end;
        TextChank := TmpStr;
      end;
    end;
  except
    //ShowMessage('FAIL!');
  end;
end;

procedure TCore.TimerReadStdOutputTimer(Sender: TObject);
begin
  ReadStdOutput(ProcessSyncthing, @AddStringToConsole, OutputChankStr);
end;

procedure TCore.TimerInitTimer(Sender: TObject);
begin
  if TimerInit.Enabled then
  begin
    TimerInit.Enabled:=false;
    actInit.Execute();
  end;
end;

procedure TCore.TimerPauseTimer(Sender: TObject);
begin
  TimerPause.Enabled:=false;
  actStart.Execute();
end;

procedure TCore.TimerStartOnStartTimer(Sender: TObject);
begin
  if OnlineTested then
  begin
    TimerStartOnStart.Enabled:=false;
    if not IsOnline then
      Start();
  end;
end;

procedure TCore.actRestartExecute(Sender: TObject);
var i: integer;
begin
  actStop.Execute();

  //todo: плохое решение! - надо делать проверку и отложенный старт
  for i:=1 to 10*5 do begin
    sleep(100); // sleep 5 second
    Application.ProcessMessages();
  end;

  actStart.Execute();
end;

procedure TCore.actExitExecute(Sender: TObject);
begin
  actStop.Execute();
  Application.Terminate();
end;

procedure TCore.actPauseExecute(Sender: TObject);
begin
  actStop.Execute();
  TimerPause.Enabled:=false;
  TimerPause.Enabled:=true;
end;

procedure TCore.actReloadConfigExecute(Sender: TObject);
begin
  self.aiohttp.Get(Core.SyncthigServer+'rest/system/config', @httpReadConfig);
end;

procedure TCore.actInitExecute(Sender: TObject);
begin
  Core.Init();

  //todo: i18n move to module main
  {strs:=TStringList.Create;
  GetFiles(ExtractFilePath(Application.ExeName)+'languages' + PathDelim, strs, '*.po');
  frmMain.cbLanguage.Items.Assign(strs);
  strs.Free;}
  SetDefaultLang('ru');
  //SetDefaultLang(GetOSLanguage()); // - just use DefaultTranslator module
  //frmMain.LanguageChanged();
end;

procedure TCore.actRunSupportProcExecute(Sender: TObject);
begin
  // empty - it is just options
end;

procedure TCore.actStartExecute(Sender: TObject);
begin
  Start();
end;

procedure TCore.actStopExecute(Sender: TObject);
begin
  Stop();
end;

procedure TCore.actTerminateExecute(Sender: TObject);
begin
  ProcessSupport.Terminate(0);
  ProcessSyncthing.Terminate(0);
end;

procedure TCore.DataModuleCreate(Sender: TObject);
begin

end;

procedure TCore.DataModuleDestroy(Sender: TObject);
begin
  Done();
end;

procedure TCore.TimerAfterStartCheckTimer(Sender: TObject);
begin
  if TimerAfterStartCheck.Enabled then
  begin
    TimerAfterStartCheck.Enabled:=False;
    if not ProcessSyncthing.Running then
    begin
      ShowMessage('Run fail. Exit code: '+IntToStr(ProcessSyncthing.ExitCode)+
        #13+
        '(press [Ctrl]+[C] to copy full error text)'+#13+
        'Run:'+#13+
        ProcessSyncthing.Executable + ' ' + ProcessSyncthing.Parameters.Text
      );
    end;
  end;
end;

procedure TCore.TimerCheckStateTimer(Sender: TObject);
begin
  //
end;

procedure TCore.Init();
var d:TDevInfo;
begin
  //todo: WIP: INIT

  Core.State := stUnknown;

  MapDevInfo := TMapDevInfo.Create();
  ListDevInfo := TStringList.Create();

  MapFolderInfo := TMapFolderInfo.Create();
  ListFolderInfo := TStringList.Create();

  IsOnline := false;

  aiohttp := TAsyncHTTP.Create;
  aiohttp.ConnectTimeout:=1000;
  aiohttp.RetryCount:=1;
  aiohttp.IOTimeout:=1000;
  aiohttp.KeepConnection:=false;
  aiohttp.OnOpened:=@aiohttpAddHeader;

  SyncthigHost:='127.0.0.1';
  SyncthigPort:=8384;
  SyncthigServer:=Format('http://%s:%d/', [SyncthigHost, SyncthigPort]);

  SyncthigExecPath:=GetSyncthigExecPath();
  SyncthigHome:=GetSyncthigHomePath();
  APIKey:=ReadAPIKeyFromCfg();

  TimerStartOnStart.Enabled:=frmOptions.chRunSyncOnStart.Checked;
  TimerPing.Enabled:=true;

  Inited := true;
end;

procedure TCore.Done();
var
  i: TMapFolderInfo.TIterator;
  i2: TMapDevInfo.TIterator;
begin
  Inited := false;

  Terminated := true;
  TimerPing.Enabled:=false;
  FreeAndNil(aiohttp);

  FreeAndNil(ListFolderInfo);
  FreeAndNil(ListDevInfo);

  //todo: FUTURE. for i in MapFolderInfo do i.Free(); - https://bugs.freepascal.org/view.php?id=35940
  i := MapFolderInfo.Iterator();
  if i <> nil then
    try
      repeat
        i.Value.Free();
      until not i.Next;
    finally
      FreeAndNil(i);
    end;

  FreeAndNil(MapFolderInfo);

  i2 := MapDevInfo.Iterator();
  if i2 <> nil then
    try
      repeat
        i2.Value.Free();
      until not i2.Next;
    finally
      FreeAndNil(i2);
    end;

  FreeAndNil(MapDevInfo);
end;

procedure TCore.EventOnline();
begin
  frmMain.shStatusCircle.Brush.Color:=clGreen;
  actReloadConfig.Execute();
end;

procedure TCore.EventOffline();
begin
  frmMain.shStatusCircle.Brush.Color:=clPurple;
end;

function TCore.ListDev_GetText(NodeIndex: Cardinal): String;
begin
  if Core.Inited and
     (NodeIndex < Self.ListDevInfo.Count)
  then
    ListDev_GetText := Self.MapDevInfo[Self.ListDevInfo[NodeIndex]].Name
  else
    ListDev_GetText := '';
end;

procedure TCore.AddStringToConsole(Str: UTF8String);
begin
  if frmMain.edConsole.Lines.Count > 100 then
    frmMain.edConsole.Lines.Delete(0);
  frmMain.edConsole.Lines.Add(Str);
end;

function TCore.GetSyncthigExecPath: UTF8String;
begin
  //OLD: result := 'D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing.exe';
  result := frmOptions.edPathToExecWithFilename.Text;
end;

function TCore.GetSyncthigHomePath: UTF8String;
begin
  //OLD: result := 'h:\Dat\syncthing\';
  result := frmOptions.edPathToConfigDir.Text;
end;

procedure TCore.FillSyncthingExecPath();
begin
  {$IFDEF WINDOWS}

  if frmOptions.chUseProxyOutputForFixBug.Checked then
  begin
    {Note:
       In windows has a bug - std output pipe is empty (for an unknown reason).
       In other program all working good (There is an error only in Syncthing).
       For fix this bug need redirect pipe stream over other program.
       (for redirect i am using 'find' program)
       (my enviroment: Win7 x64, Syncthing v0.14.27 for Windows (32 bit).
    }
    ProcessSyncthing.Executable :=
      GetEnvironmentVariable('WINDIR')+'\system32\cmd.exe';
    ProcessSyncthing.Parameters.Text :=
      '/C "' + SyncthigExecPath + ' ' +
      //'--no-console '+
      '--no-browser '+
      '--home=' + SyncthigHome + ' ' +
      ' | find /v " empty_find_just_for_redirect_stdio_00686558 " "';
  end else
  begin
    ProcessSyncthing.Executable := SyncthigExecPath;
    ProcessSyncthing.Parameters.Text :=
      //'--no-console '+
      '--no-browser '+
      '--home=' + SyncthigHome;
  end;

  {$ELSE}
  //todo: WIP: FillSyncthingExecPath linux
  ProcessSyncthing.Executable := FindSyncthigPath();
  ProcessSyncthing.Parameters.Clear;
  ProcessSyncthing.Parameters.Add('--home=' + SyncthigHome);
  ProcessSyncthing.Parameters.Add('--no-browser');
  {$ENDIF}
end;

procedure TCore.FillSupportExecPath();
begin
  //todo: WIP: FillSupportExecPath
  ProcessSupport.Executable := 'D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing-inotify.exe';
  ProcessSupport.Parameters.Text := '--home=' + SyncthigHome;
end;

end.

