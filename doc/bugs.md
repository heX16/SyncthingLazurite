
2025-11-06 20:35:14

после выходи из syncthing,
sync.laz. считает что он онлайн.
и попадает в какойто бесконечный цикл переподключений.

[20:31:55] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:31:55] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8383/rest/events?since=3629&limit=10&timeout=60
[20:31:56] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:31:56] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:31:56] FSyncthingAPI.OnStateChanged: ssOnline
[20:31:56] FSM_Process: State=ssOnline, Command=ssCmdLongPollingConnect (repeat)
[20:31:57] HTTP_EventAPI: Received event data, StatusCode=200
[20:33:00] FSM_Process: State=ssOnline, Command=ssCmdLongPollingForceRestart
[20:33:00] FSyncthingAPI.OnStateChanged: ssOnlineLongPollingWait
[20:33:00] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingForceRestart (repeat)
[20:33:01] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:33:01] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8383/rest/events?since=3629&limit=10&timeout=60
[20:33:01] LongPollingError: HTTP Error Code=16009, URL=http://127.0.0.1:8383/rest/events?since=3629&limit=10&timeout=60
[20:33:01] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingError
[20:33:01] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:33:01] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8383/rest/events?since=3629&limit=10&timeout=60
[20:33:02] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:33:02] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:33:02] LongPollingError: HTTP Error Code=16008, URL=http://127.0.0.1:8383/rest/events?since=3629&limit=10&timeout=60
[20:33:02] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingError
[20:33:02] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingErrorDisconnected
[20:33:03] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:33:03] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8383/rest/events?since=3629&limit=10&timeout=60
[20:33:03] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:33:04] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:33:04] LongPollingError: HTTP Error Code=16008, URL=http://127.0.0.1:8383/rest/events?since=3629&limit=10&timeout=60
[20:33:04] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingError
[20:33:04] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingErrorDisconnected
[20:33:04] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:33:04] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8383/rest/events?since=3629&limit=10&timeout=60
[20:33:05] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:33:05] FSM_Process: State=ssOnlineLongPollingWait, Command=ssCmdLongPollingConnect
[20:33:05] LongPollingError: HTTP Error Code=16008, URL=http://127.0.0.1:8383/rest/events?since=3629&limit=10&timeout=60









---------------------------




NOTE: кажется этот баг уже исправлен. нужно еще раз проверить.

При закрытии syncthing, наша программма не замечает отключения.
Вообще не замечает.
И даже таймаут на подключение к long pooling не срабатывает.
Тоесть:
1. в момент отключения syncthing, наша программа не выдает никаких сообщений и делает вид что все еще подключенна.
2. через 60 секунд должен произойти реконнект long pooling соединения к syncthing, и в теории в этот момент программа заметит что отключенна.
но ничего не происходит.
через 60 секунд, и даже через 10 минут - программа не выдает никаких сообщений и делает вид что все еще подключенна.

Если нажать кнопку "отключиться" то произойдет правильное отключение.
Если затем нажать кнопку "подключиться" то произойдет правильная попытка подключения с сообщением об ошибке.
Это значит что функция disconnect и все функции связанные с этой функцией работают корректно.

Нужно писать юнит тесты для AsyncHTTP. Возможно проблема в этой библиотеке.

-----------------------------

NOTE: кажется этот баг уже исправлен. нужно еще раз проверить.

Connect:
  FSM_Process: State=ssOffline, Command=ssCmdConnect
  FSyncthingAPI.OnStateChanged: ssConnectingInitAndPing
  FSM_Process: State=ssConnectingInitAndPing, Command=ssCmdConnect
  FSyncthingAPI.OnStateChanged: ssConnectingPingWait
  FSM_Process: State=ssConnectingPingWait, Command=ssCmdConnectingPingAck
  FSyncthingAPI.OnStateChanged: ssConnectingWaitData
  FSyncthingAPI.OnTreeChanged: epConfig
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epSystem_Connections
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epStats_Device
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epStats_Folder
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epSystem_Status
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epSystem_Version
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
Online:
  FSyncthingAPI.OnStateChanged: ssOnline
LongPolling reconnect:
  FSM_Process: State=ssOnline, Command=ssCmdLongPollingTimeToAutoReconnect
  RestartLongPolling: Restarting long-polling connection
  StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=0&limit=10&timeout=60
  HTTP_EventAPI: Received event data, StatusCode=200
  HTTP_EventAPI: Parsed 10 events
  FSyncthingAPI.OnTreeChanged: epStats_Folder
  FSM_Process: State=ssOnline, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epStats_Folder
  FSM_Process: State=ssOnline, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epSystem_Status
  FSM_Process: State=ssOnline, Command=ssCmdDataReceived
LongPolling reconnect:
  FSM_Process: State=ssOnline, Command=ssCmdLongPollingTimeToAutoReconnect
  RestartLongPolling: Restarting long-polling connection

LongPolling fault:
  StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1053&limit=10&timeout=60
  LongPollingError: HTTP Error Code=16009 HTTPErrorCode_SocketIOTimeout, URL=http://127.0.0.1:8384/rest/events?since=1053&limit=10&timeout=60

OnlineUnstable:
  FSM_Process: State=ssOnline, Command=ssCmdLongPollingError
  FSyncthingAPI.OnStateChanged: ssOnlineUnstable
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
  RestartLongPolling: Restarting long-polling connection
  StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1053&limit=10&timeout=60
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
  RestartLongPolling: Restarting long-polling connection
  StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1053&limit=10&timeout=60
  LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1053&limit=10&timeout=60
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
  RestartLongPolling: Restarting long-polling connection
  StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1053&limit=10&timeout=60
  LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1053&limit=10&timeout=60
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
  RestartLongPolling: Restarting long-polling connection
  StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1053&limit=10&timeout=60
  LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1053&limit=10&timeout=60
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
  RestartLongPolling: Restarting long-polling connection

................

FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1056&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1056&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1056&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1056&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected

...................

RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1197&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected

------------------

HTTP_EventAPI: Received event data, StatusCode=200
HTTP_EventAPI: Parsed 2 events
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1199&limit=10&timeout=60
HTTP_EventAPI: Received event data, StatusCode=200
HTTP_EventAPI: Parsed 3 events
FSyncthingAPI.OnTreeChanged: epStats_Device
FSM_Process: State=ssOnlineUnstable, Command=ssCmdDataReceived
FSyncthingAPI.OnTreeChanged: epStats_Folder
FSM_Process: State=ssOnlineUnstable, Command=ssCmdDataReceived
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1202&limit=10&timeout=60
HTTP_EventAPI: Received event data, StatusCode=200
HTTP_EventAPI: Parsed 10 events
FSyncthingAPI.OnTreeChanged: epStats_Folder
FSM_Process: State=ssOnlineUnstable, Command=ssCmdDataReceived
FSyncthingAPI.OnTreeChanged: epStats_Folder
FSM_Process: State=ssOnlineUnstable, Command=ssCmdDataReceived
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1219&limit=10&timeout=60
HTTP_EventAPI: Received event data, StatusCode=200
HTTP_EventAPI: Parsed 10 events
FSyncthingAPI.OnTreeChanged: epStats_Folder
FSM_Process: State=ssOnlineUnstable, Command=ssCmdDataReceived
FSyncthingAPI.OnTreeChanged: epStats_Folder
FSM_Process: State=ssOnlineUnstable, Command=ssCmdDataReceived
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1238&limit=10&timeout=60
HTTP_EventAPI: Received event data, StatusCode=200
HTTP_EventAPI: Parsed 1 events
FSyncthingAPI.OnTreeChanged: epSystem_Connections
FSM_Process: State=ssOnlineUnstable, Command=ssCmdDataReceived
FSyncthingAPI.OnTreeChanged: epStats_Device
FSM_Process: State=ssOnlineUnstable, Command=ssCmdDataReceived
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1239&limit=10&timeout=60
HTTP_EventAPI: Received event data, StatusCode=200
HTTP_EventAPI: Parsed 10 events
FSyncthingAPI.OnTreeChanged: epStats_Folder
FSM_Process: State=ssOnlineUnstable, Command=ssCmdDataReceived
FSyncthingAPI.OnTreeChanged: epStats_Folder
FSM_Process: State=ssOnlineUnstable, Command=ssCmdDataReceived
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1277&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
RestartLongPolling: Restarting long-polling connection
StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1277&limit=10&timeout=60
LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1277&limit=10&timeout=60
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected

------------------------

NOTE: кажется этот баг уже исправлен. нужно еще раз проверить.

Disconnect:
  FSM_Process: State=ssOnlineUnstable, Command=ssCmdDisconnect
  FSyncthingAPI.OnStateChanged: ssDisconnecting
  FSM_Process: State=ssDisconnecting, Command=ssCmdDisconnect

Offline:
  FSyncthingAPI.OnStateChanged: ssOffline
  LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1277&limit=10&timeout=60
  FSM_Process: State=ssOffline, Command=ssCmdLongPollingError
  FSM_Process: State=ssOffline, Command=ssCmdLongPollingDisconnected
Connect:
  FSM_Process: State=ssOffline, Command=ssCmdConnect
  FSyncthingAPI.OnStateChanged: ssConnectingInitAndPing
  FSM_Process: State=ssConnectingInitAndPing, Command=ssCmdConnect
  FSyncthingAPI.OnStateChanged: ssConnectingPingWait
PingFault!: (WTF???)
  FSM_Process: State=ssConnectingPingWait, Command=ssCmdConnectingPingFault
  FSyncthingAPI.OnStateChanged: ssOffline
PollingTimerRestore - DOUBLE!!!
  FSM_Process: State=ssOffline, Command=ssCmdLongPollingTimerRestore
  FSM_Process: State=ssOffline, Command=ssCmdLongPollingTimerRestore
Disconnect:
  FSM_Process: State=ssOffline, Command=ssCmdDisconnect
  FSM_Process: State=ssOffline, Command=ssCmdDisconnect
Connect:
  FSM_Process: State=ssOffline, Command=ssCmdConnect
  FSyncthingAPI.OnStateChanged: ssConnectingInitAndPing
  FSM_Process: State=ssConnectingInitAndPing, Command=ssCmdConnect
  FSyncthingAPI.OnStateChanged: ssConnectingPingWait
  FSM_Process: State=ssConnectingPingWait, Command=ssCmdConnectingPingAck
  FSyncthingAPI.OnStateChanged: ssConnectingWaitData
  FSyncthingAPI.OnTreeChanged: epConfig
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epSystem_Connections
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epStats_Device
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epStats_Folder
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epSystem_Status
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
  FSyncthingAPI.OnTreeChanged: epSystem_Version
  FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
Online:
  FSyncthingAPI.OnStateChanged: ssOnline
LongPolling:
  FSM_Process: State=ssOnline, Command=ssCmdLongPollingTimeToAutoReconnect
  RestartLongPolling: Restarting long-polling connection
  StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1277&limit=10&timeout=60
  FSM_Process: State=ssOnline, Command=ssCmdLongPollingTimerRestore
  HTTP_EventAPI: Received event data, StatusCode=200
  HTTP_EventAPI: Parsed 1 events
  FSyncthingAPI.OnTreeChanged: epStats_Device
  FSM_Process: State=ssOnline, Command=ssCmdDataReceived
Online - ALL OK


-------------------------

NOTE: кажется этот баг уже исправлен. нужно еще раз проверить.

Connect:
[04:55:16] FSM_Process: State=ssOffline, Command=ssCmdConnect
[04:55:16] FSyncthingAPI.OnStateChanged: ssConnectingInitAndPing
[04:55:16] FSM_Process: State=ssConnectingInitAndPing, Command=ssCmdConnect
[04:55:16] FSyncthingAPI.OnStateChanged: ssConnectingPingWait
[04:55:16] FSM_Process: State=ssConnectingPingWait, Command=ssCmdConnectingPingAck
[04:55:16] FSyncthingAPI.OnStateChanged: ssConnectingWaitData
[04:55:16] FSyncthingAPI.OnTreeChanged: epConfig
[04:55:16] FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
[04:55:16] FSyncthingAPI.OnTreeChanged: epSystem_Connections
[04:55:16] FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
[04:55:16] FSyncthingAPI.OnTreeChanged: epStats_Device
[04:55:16] FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
[04:55:16] FSyncthingAPI.OnTreeChanged: epStats_Folder
[04:55:16] FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
[04:55:16] FSyncthingAPI.OnTreeChanged: epSystem_Status
[04:55:16] FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
[04:55:16] FSyncthingAPI.OnTreeChanged: epSystem_Version
[04:55:16] FSM_Process: State=ssConnectingWaitData, Command=ssCmdDataReceived
Online:
[04:55:16] FSyncthingAPI.OnStateChanged: ssOnline
LongPolling:
[04:55:16] FSM_Process: State=ssOnline, Command=ssCmdLongPollingTimeToAutoReconnect
[04:55:16] RestartLongPolling: Restarting long-polling connection
[04:55:16] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=0&limit=10&timeout=60
[04:55:16] HTTP_EventAPI: Received event data, StatusCode=200
[04:55:16] HTTP_EventAPI: Parsed 10 events
[04:55:16] FSyncthingAPI.OnTreeChanged: epSystem_Status
[04:55:16] FSM_Process: State=ssOnline, Command=ssCmdDataReceived
[04:55:16] FSyncthingAPI.OnTreeChanged: epSystem_Status
[04:55:16] FSM_Process: State=ssOnline, Command=ssCmdDataReceived
[04:56:16] FSM_Process: State=ssOnline, Command=ssCmdLongPollingTimeToAutoReconnect
LongPolling:
[04:56:16] RestartLongPolling: Restarting long-polling connection
LongPolling FAIL:
[04:56:16] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
[04:56:16] LongPollingError: HTTP Error Code=16009, URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
[04:56:16] FSM_Process: State=ssOnline, Command=ssCmdLongPollingError
Unstable:
[04:56:16] FSyncthingAPI.OnStateChanged: ssOnlineUnstable
[04:56:16] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
[04:56:21] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
LongPolling:
[04:56:21] RestartLongPolling: Restarting long-polling connection
[04:56:21] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
LongPolling: - !!!
[04:56:26] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
[04:56:26] RestartLongPolling: Restarting long-polling connection
[04:56:26] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
[04:56:26] LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
[04:56:26] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
[04:56:26] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
[04:56:31] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
[04:56:31] RestartLongPolling: Restarting long-polling connection
[04:56:31] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
[04:56:31] LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
[04:56:31] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
[04:56:31] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
[04:56:36] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
[04:56:36] RestartLongPolling: Restarting long-polling connection
[04:56:36] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
[04:56:36] LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
[04:56:36] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
[04:56:36] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
[04:56:41] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
[04:56:41] RestartLongPolling: Restarting long-polling connection
[04:56:41] StartLongPolling: Starting long-polling request to URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
[04:56:41] LongPollingError: HTTP Error Code=16499, URL=http://127.0.0.1:8384/rest/events?since=1350&limit=10&timeout=60
[04:56:41] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingError
[04:56:41] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingDisconnected
[04:56:46] FSM_Process: State=ssOnlineUnstable, Command=ssCmdLongPollingTimerRestore
[04:56:46] RestartLongPolling: Restarting long-polling connection
