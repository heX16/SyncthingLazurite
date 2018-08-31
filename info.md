Misc
-------

procedure TfrmMain.httpTest(Query: THttpQuery);
begin
  //example: aiohttp.Get('http://ya.ru', @httpTest);
  if Query.CallState = httpLoad then
    ShowMessage(IntToStr(Query.Status));
end;


http://wiki.freepascal.org/Executing_External_Programs
http://wiki.freepascal.org/Executing_External_Programs/ru

%WINDIR%\system32
%comspec% (C:/windows/system32/cmd.exe)

WORK: /C "D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing.exe | find /v " FAKE_FILTER_STRING_FOR_FIND_COMMAND_AS_MIRROR_REDIRECT " "

C:\Windows\System32\cmd.exe
/C "D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing.exe"
/C "D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing_hex_disk.bat"

function GetConsoleWindow: HWND; stdcall; external kernel32;

procedure ShowSelfConsoleWindow;
begin
  //show the console window
  ShowWindow(GetConsoleWindow(), SW_SHOW);
end;

procedure HideSelfConsoleWindow;
begin
  //hide the console window
  ShowWindow(GetConsoleWindow(), SW_HIDE);
end;




System Endpoints
-------------------
    GET /rest/system/config
    GET /rest/system/config/insync
    GET /rest/system/connections
    GET /rest/system/debug
    GET /rest/system/discovery
    GET /rest/system/error
    GET /rest/system/log
    GET /rest/system/ping
    GET /rest/system/status
    GET /rest/system/upgrade
    GET /rest/system/version

    POST /rest/system/ping
    POST /rest/system/reset
    POST /rest/system/restart
    POST /rest/system/shutdown
    POST /rest/system/upgrade

Database Endpoints
----------------------
    GET /rest/db/browse
    GET /rest/db/completion
    GET /rest/db/file
    GET /rest/db/ignores
    GET /rest/db/need
    GET /rest/db/status

Event Endpoints
------------------
    GET /rest/events

    since=<lastSeenID> events=EventTypeA
     {
     "time" : "2017-03-06T23:58:21.844739891+01:00",
     "globalID" : 123,
     "data" : {
        "type" : "file",
        "action" : "deleted",
        "path" : "/media/ntfs_data/Dokumente/testfile",
        "label" : "Dokumente",
        "folderID" : "Dokumente",
        "modifiedBy" : "BPDFDTU"
     },
     "type" : "RemoteChangeDetected",
     "id" : 2
     }

Statistics Endpoints
-----------------------
    GET /rest/stats/device
      {
        "HJBNI74-LB5I7ND-IDXKOJH-CMM5KM3-AR4BMVB-XOXIJAB-FSYCOFN-3EBH7AS" : {
          "lastSeen" : "1970-01-01T03:00:00+03:00"
        },
        ...
    GET /rest/stats/folder

Misc Services Endpoints
---------------------------
    GET rest/svc/deviceid
    GET rest/svc/lang
    GET rest/svc/random/string
    GET rest/svc/report




rest/system/connections
---------------------------

    "connections" : {
      "67SBX7B-MYCRFDG-Q3UE7N5-FO3JYZF-CAWIBWR-X54RIY3-HHVFZJW-4IEF2QY" : {
        "address" : "5.164.194.12:16221",
        "at" : "2018-08-31T09:13:38.8356001+03:00",
        "clientVersion" : "v0.14.48",
        "connected" : true,
        "inBytesTotal" : 565,
        "outBytesTotal" : 787,
        "paused" : false,
        "type" : "tcp-client"
      },



detect "Up to Date"
----------------------

    https://forum.syncthing.net/t/syncthing-is-the-best-help-me-detect-up-to-date-with-rest/10968/5

    /rest/db/completion?folder=src&device=[specific_device_id]

    If I do that, I get:

    {
      "completion": 99.14897162188355,
      "globalBytes": 78764706,
      "needBytes": 670310,
      "needDeletes": 0
    }

    This means I have to iterate over every directory and every device to see problems!

    In particular,

    /rest/db/status?folder=src

    shows:

    {
      "globalBytes": 78764706,
      "globalDeleted": 3188,
      "globalDirectories": 977,
      "globalFiles": 4999,
      "globalSymlinks": 12,
      "ignorePatterns": false,
      "inSyncBytes": 78764706,
      "inSyncFiles": 4999,
      "invalid": "",
      "localBytes": 78764706,
      "localDeleted": 3147,
      "localDirectories": 977,
      "localFiles": 4999,
      "localSymlinks": 12,
      "needBytes": 0,
      "needDeletes": 0,
      "needDirectories": 0,
      "needFiles": 0,
      "needSymlinks": 0,
      "sequence": 44122,
      "state": "idle",
      "stateChanged": "2017-11-17T18:55:14.3305827-08:00",
      "version": 44122
    }

