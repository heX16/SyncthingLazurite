### Обзор проекта SyncthingLazurite (заметки для работы)

### Содержание
- 1. Назначение
- 2. Архитектура и модули
- 3. JSONTree — единая модель данных
- 4. Подробности модуля syncthing_api.pas
- 5. Потоки и модель данных
- 6. Сетевая модель
- 7. UI‑детали
- 8. Известные WIP/задачи
- 9. Сборка/запуск (кратко)

### 1. Назначение
Настольное приложение (Lazarus/FPC) для подключения к локальному Syncthing по REST/Event API. 
Отображает интерфейс для отображения что происходит с Syncthing, отображает устройства/папки/события в UI.

При подключении выкачивает JSON дерево (JSONTree) которое отображает состояние Syncthing, подписывается на обновления, и затем поддерживает в своей памяти актуальное JSON дерево.

### 2. Архитектура и модули
- `syncthing_api.pas` — ядро взаимодействия с Syncthing:
  - FSM состояния: ssOffline → ssConnecting* → ssOnline / ssOnlinePaused / ssOnlineUnstable → ssDisconnecting.
  - Два HTTP‑клиента: REST (`FHTTP`) и long‑polling событий (`FHTTPEvents`).
  - Загружает «базовые» эндпоинты при коннекте: config, system/connections, stats/device, stats/folder, system/status, system/version.
  - Обрабатывает события (`/rest/events`) пакетно; по типам событий дозаказывает ресурсы.
  - Коллбеки наружу: OnConnected, OnEvent, OnTreeChanged(Path/EndpointId), OnStateChanged и др.

- `AsyncHTTP.pas` — очередь асинхронных HTTP‑запросов:
  - Worker‑поток, `TFPHTTPClient`; таймауты, ретраи, keep‑alive.
  - Коллбеки и служебные события исполняются в главном потоке (`Synchronize`).

- `uModuleMain.pas` — главный модуль GUI:
  - Создает `TSyncthingAPI`, подписывается на события.
  - Обновляет `treeDevices`, `treeFolders` по изменениям `FTreeRoot`.
  - Подсказка трея (онлайн‑устройства; локальные адреса помечаются `(local)`).

- `uFormMain.pas` — главная форма (деревья устройств/папок, лог событий, индикатор статуса).
- `uFormOptions.pas` — диалог настроек (пути/ключи/флаги).
- `uFormJsonView.pas` — просмотр JSON (HTTP через `AsyncHTTP`, подсказочные поля).
- `uModuleCore.pas` — чтение API‑ключа из `config.xml`, подготовка запуска Syncthing (частично WIP), таймеры/консоль.
- Утилиты: `syncthing_api_utils.pas`, `usyncthingtypes.pas`, `uutils.pas`, `vtutils.pas`, `ulogging.pas`.

### 3. JSONTree — единая модель данных
Определение
- JSONTree — это единое корневое `TJSONObject` (поле `FTreeRoot`) со всей актуальной информацией Syncthing, которую приложение поддерживает в памяти.

Инициализация и структура
- При создании `TSyncthingAPI` дерево инициализируется дефолтной структурой (`CreateDefaultRoot`/`GetDefaultRootStr`).
- Дальше, при подключении, данные подкачиваются из REST‑эндпоинтов. Каждому эндпоинту соответствует «путь» в дереве (URI с заменой `/` на `.`). Примеры:
  - `system/version` → путь `system.version`
  - `config/folders` → путь `config.folders`

Запись данных
- Сетевые ответы пишутся в дерево атомарно: `JSONTreeNewDataFromNetwork(JsonPath, NewData)` →
  - `SetJsonNodeAtPath` создаёт/находит родительские объекты по пути и заменяет целевую ветку на новые данные;
  - обновляются «типизированные указатели» (см. ниже);
  - рассылается уведомление `NotifyTreeChanged(JsonPath, EndpointId)`.
- Особый случай: когда обновляется корневая конфигурация `config`, вызывается `UpdateAllJsonPointersFromTree` для пересборки всех указателей.

Типизированные указатели (кэш быстрых ссылок)
- Для ускорения доступа к часто используемым веткам поддерживаются указатели‑поля класса:
  - `config: TJSONObject`
  - `config_folders: TJSONArray`
  - `config_devices: TJSONArray`
  - `config_options: TJSONObject`
  - `stats_device: TJSONArray`
  - `stats_folder: TJSONArray`
- Они обновляются в `UpdateJsonPointersFromTree` путём поиска в `FTreeRoot` по «точечным» путям.

Доступ к значениям
- Для произвольного чтения используйте `FTreeRoot.FindPath('system.version.version')` и т.п.
- Массивы/объекты возвращаются как `TJSONArray`/`TJSONObject`; строки/числа/булевы — как `TJSONData` с соответствующим `JSONType`.

Потокобезопасность и события
- Перед модификацией вызывается `OnBeforeTreeModify`, после — `OnAfterTreeModify` (см. раздел про потоки). UI читает дерево под защитой `LockJSONTree`.
- `OnTreeChanged(EndpointId, Path)` сигнализирует, какая ветка была обновлена; GUI по нему обновляет списки устройств/папок и подсказки.

Практические заметки
- Не освобождать объекты/массивы, на которые указывают типизированные поля — владельцем является `FTreeRoot`.
- При необходимости одновременного чтения нескольких веток, брать `LockJSONTree` один раз и работать с локальными ссылками.
- Если ветка отсутствует (nil), это нормально до прихода соответствующего REST‑ответа.

Расширение
- Добавление новых REST‑ресурсов сводится к сопоставлению `EndpointId` ↔ `URI` и использованию `LoadEndpoint`; ветка автоматически попадёт в JSONTree по правилу путей.

### 4. Подробности модуля syncthing_api.pas
Роль и ответственность
- Единая точка интеграции с REST и Event API Syncthing.
- Управляет жизненным циклом подключения через конечный автомат (FSM).
- Поддерживает единую модель данных (JSONTree) и уведомляет слушателей об изменениях.

Состояния (FSM) и команды
- Состояния: `ssOffline`, `ssConnectingInitAndPing`, `ssConnectingPingWait`, `ssConnectingWaitData`, `ssOnline`, `ssOnlinePaused`, `ssOnlineUnstable`, `ssDisconnecting`.
- Команды: `ssCmdConnect/Disconnect`, `ssCmdPause/PauseRelease`, `ssCmdConnectingPingAck/Fault/Timeout`, `ssCmdDataReceived`, `ssCmdLongPolling*`, `ssCmdConnectionStable/Unstable`.
- Метод `FSM_Process` отрабатывает переходы, запускает и останавливает long‑polling, инициирует загрузки эндпоинтов и вызывает коллбеки.

HTTP‑клиенты
- `FHTTP` — REST: очередь запросов, единый обработчик `HTTPHandle_RestAPI`.
- `FHTTPEvents` — Event API: long‑polling; обработчики разрывов/ошибок, автоперезапуск по таймеру.
- Заголовок `X-API-Key` добавляется в `HttpAddHeader` при `OnOpened`.

JSON‑дерево и указатели
- Инициализация: `CreateDefaultRoot` (+ `GetDefaultRootStr`).
- Запись веток: `SetJsonNodeAtPath` (с предварительным `FindAndCreatePathInJsonTree`).
- Обновление указателей: `UpdateJsonPointersFromTree` (+ `UpdateAllJsonPointersFromTree`).
- Высылка уведомлений: `JSONTreeNewDataFromNetwork` → `NotifyTreeChanged`.

Инициализационная загрузка и эндпоинты
- Базовые ресурсы: массив `SyncthingEndpointsBasic`.
- Загрузка: `LoadAllBasicEndpoints` / `LoadEndpoint(Id)`.
- Таблица соответствий: `GetEndpointURI`, обратное сопоставление `GetEndpointIdByURI` (включая под‑ресурсы `.../@`).

Long‑polling событий
- Запуск/стоп/перезапуск: `StartLongPolling`, `StopLongPolling`, `RestartLongPolling` (+ таймер‑интервал).
- Обработка: `HTTPHandle_EventAPI` → `HandleIncommingDataFromEventAPI` → `ProcessEvent` → дозагрузка нужных ресурсов.
- Интеграция фрагментов в дерево зарезервирована `IntegrateEvent` (пока пустышка — логика сведена к дозагрузкам).

Публичный API
- Настройки подключения: `SetEndpoint`, `SetAPIKey`, таймауты (`ConnectTimeout`, `IOTimeout`, `ConnectingTimeout`, период перезапуска long‑polling).
- Управление соединением: `Connect`, `Disconnect`, `Pause`, `PauseRelease`, синхронный `Ping()`.
- Состояние: `State`, `IsOnline`, `Command`, данные: `TreeRoot` и типизированные указатели (`config_*`, `stats_*`).
- Коллбеки: `OnBeforeConnect/OnConnected/OnConnectError/OnBeforeDisconnect/OnDisconnectedByUser/OnHardDisconnect`, события long‑polling, дерево, смена состояния.

Ошибки/восстановление
- Таймауты соединения/ввода‑вывода, авто‑ретраи в `AsyncHTTP`.
- Коды псевдо‑ошибок для некоторых ситуаций (например, разрыв keep‑alive) и повторная попытка.
- При ошибках long‑polling → состояние `ssOnlineUnstable`, далее восстановление по таймеру/событию.

Потокобезопасность
- Сетевые коллбеки приходят через механизм очереди/worker → `Synchronize`.
- Перед модификацией дерева вызывается `OnBeforeTreeModify`, после — `OnAfterTreeModify`; UI бережно читает дерево под `LockJSONTree`.

Расширяемость и заметки
- Простое сопоставление эндпоинтов позволяет добавлять новые ресурсы без изменения основной логики (через `GetEndpointCallback`/`GetEndpointURI`).
- Возможна интеграция инкрементального применения фрагментов событий в дерево (реализовать в `IntegrateEvent`).

### 5. Потоки и модель данных
- Единственный источник правды — `TSyncthingAPI.FTreeRoot`. Любые обновления приходят из сетевых коллбеков.
- Перед модификацией вызывается `OnBeforeTreeModify`, после — `OnAfterTreeModify`.
- `uModuleMain` использует `TCriticalSection` (`LockJSONTree`) для синхронизации отрисовки/чтения.

### 6. Сетевая модель
- REST: инициализационная загрузка «базовых» эндпоинтов, далее точечные дозагрузки по событиям.
- Events: long‑polling `/rest/events?since=<id>&limit=10&timeout=60` с автоперезапуском по таймеру или при сбоях.
- Заголовок `X-API-Key` добавляется в `OnOpened` каждого запроса.

### 7. UI‑детали
- Иконки устройств: 0=offline, 1=online, 2=paused. Онлайновость проверяется по `system.connections`.
- Подсказка трея — список онлайновых устройств (до N), «(local)» помечается по `IsLocalIP`.
- Индикатор круга меняет цвет в зависимости от `TSyncthingFSM_State`.

### 8. Известные WIP/задачи
- `uModuleCore` - устаревший модуль. Функционал будет перенесен. Еще не переперенос логики запуска/остановки Syncthing.
- `uModuleCore` не задействован в новой схеме.
- `uFormJsonView` содержит тестовые методы (mnTestConnect/mnTestJson).
- Не реализованы действия «Open Web UI» и часть обработчиков.

### 9. Сборка/запуск (кратко)
- Lazarus/FPC, открыть `SimpleSyncthing.lpi`, собрать.
- В настройках приложения указать путь к `syncthing.exe`, путь к каталогу с `config.xml` и/или API‑ключ.
- Подключиться и наблюдать обновления.


