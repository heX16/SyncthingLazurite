unit uSyncthingTypes;

{$mode ObjFPC}{$H+}

interface

uses
  gqueue,
  ghashmap,
  fpjson,
  HashMapStr, // THashFuncString
  Classes, SysUtils;

type
  { TDevInfo }
  TDevId = string;
  TFolderId = string;

  // device info 'record'
  TDevInfo = class
    Json: TJSONObject;
    Id: TDevId;
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
    Id: TFolderId;
    Path: string;

    constructor Create(SetJson: TJSONObject);
    destructor Destroy(); override;
    procedure Update(NewJson: TJSONObject); virtual;
    function DirectoryExists(): Boolean;
  end;


type
  TSyncthingEventQueue = specialize TQueue<TJSONObject>;
  TMapDevInfo = specialize THashMap<TDevId, TDevInfo, THashFuncString>;
  TMapFolderInfo = specialize THashMap<TFolderId, TFolderInfo, THashFuncString>;

implementation

uses
  LazFileUtils;

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



end.

