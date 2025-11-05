### Обзор проекта SyncthingLazurite (заметки для работы)

### Содержание
- Назначение
- Назначение
- Архитектура и модули
- JSONTree — единая модель данных
- Подробности модуля syncthing_api.pas
- Потоки и модель данных
- Сетевая модель
- UI‑детали
- Известные WIP/задачи
- Сборка/запуск (кратко)

-----------------------------------------
### Назначение файла

**Этот файл служит долговременной памятью для GPT ассистента и всех его будущих экземпляров.**

Важная информация:
- Кратковременная память GPT стирается при перезапуске сессии диалога.
- Этот файл сохраняет критически важную информацию о проекте между сессиями диалогов.
- Здесь хранится документация, особенности архитектуры и важные детали реализации.
- GPT ассистент должен обновляеть файл по мере развития проекта и получения новой информации.

-----------------------------------------
### Назначение
Кроссплатформенное настольное приложение (Lazarus/FPC) для подключения к локальному Syncthing по REST/Event API. Поддерживает Windows, Linux, macOS.
Отображает интерфейс для отображения что происходит с Syncthing, отображает устройства/папки/события в UI.

При подключении выкачивает JSON дерево (JSONTree) которое отображает состояние Syncthing, подписывается на обновления, и затем поддерживает в своей памяти актуальное JSON дерево.

**Важно:** Проект должен оставаться кроссплатформенным. Не используйте платформо-зависимые решения (WinAPI, Linux-специфичные вызовы и т.д.). Применяйте только LCL/FPC компоненты и функции, работающие на всех целевых платформах.

-----------------------------------------
### Архитектура и модули
- `syncthing_api.pas` — ядро взаимодействия с Syncthing:
  - FSM состояния: ssOffline → ssConnecting* → ssOnline / ssOnlinePaused / ssOnlineUnstable → ssDisconnecting.
  - Два HTTP‑клиента: REST (`FHTTP`) и long‑polling событий (`FHTTPEvents`).
  - Загружает «базовые» эндпоинты при коннекте: config, system/connections, stats/device, stats/folder, system/status, system/version.
  - Обрабатывает события (`/rest/events`) пакетно; по типам событий дозаказывает ресурсы.
  - Коллбеки наружу: OnConnected, OnEvent, OnTreeChanged(Path/EndpointId), OnStateChanged и др.

- `uSyncthingManager.pas` — управление процессом "Syncthing":
  - Класс `TSyncthingManager` наследуется от `TSyncthingAPI` и добавляет функционал запуска программы "syncthing".
  - Запуск/остановка процесса "syncthing".
  - Машина состояний (FMS): `psUnknown` → `psStarting` → `psRunning` → `psStopping` → `psStopped`.
  - Извлечение настроек (включая API‑ключ) из `config.xml` файла.
  - События: `OnProcessStateChanged` для отслеживания состояния процесса, `OnConsoleOutput` для вывода консоли.

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
- `uModuleCore.pas` — чтение API‑ключа из `config.xml`, подготовка запуска Syncthing (частично WIP), таймеры/консоль. Устаревший модуль. Подлежит замене и последующему удалению (в будущем). Функционал будет переноситься в другие модули.
- Утилиты: `syncthing_api_utils.pas`, `usyncthingtypes.pas`, `uutils.pas`, `vtutils.pas`, `ulogging.pas`.

-----------------------------------------
### JSONTree — единая модель данных
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

-----------------------------------------
### Подробности модуля syncthing_api.pas

**Основная цель:** Модуль обеспечивает взаимодействие с Syncthing через REST API и Event API, поддерживая единое JSON-дерево состояния.

**Архитектура на высоком уровне:**

1. **Класс TSyncthingAPI** - центральный компонент для управления подключением к Syncthing

2. **Конечный автомат (FSM)** состояний подключения:
   - `ssOffline` → `ssConnecting*` → `ssOnline*` → `ssDisconnecting`
   - Управляет жизненным циклом соединения

3. **Два HTTP-клиента:**
   - `FHTTP` - для REST запросов (синхронные)
   - `FHTTPEvents` - для long-polling событий (асинхронные)

4. **JSON-дерево данных:**
   - `FTreeRoot` - единое корневое TJSONObject со всей информацией
   - Типизированные указатели (`config`, `stats_device`, `stats_folder` и др.)
   - Автоматическая синхронизация при обновлениях

5. **Система эндпоинтов:**
   - Перечисление `TSyncthingEndpointId` всех доступных REST эндпоинтов
   - Автоматическая загрузка базовых данных при подключении

6. **События и коллбеки:**
   - Различные события (`OnConnected`, `OnEvent`, `OnTreeChanged`, `OnStateChanged`)
   - Потокобезопасные вызовы через `Synchronize`

**Ключевые возможности:**
- Подключение/отключение с автоматическим восстановлением
- Синхронный ping для проверки доступности
- Асинхронное получение событий через long-polling
- Автоматическое обновление данных по событиям
- Потокобезопасная работа с GUI

Модуль спроектирован как современная замена старому `uModuleCore.pas`.

#### Роль и ответственность
- Единая точка интеграции с REST и Event API Syncthing.
- Управляет жизненным циклом подключения через конечный автомат (FSM).
- Поддерживает единую модель данных (JSONTree) и уведомляет слушателей об изменениях.

#### Состояния (FSM) и команды
**Состояния:** `ssOffline`, `ssConnectingInitAndPing`, `ssConnectingPingWait`, `ssConnectingWaitData`, `ssOnline`, `ssOnlinePaused`, `ssOnlineUnstable`, `ssOnlineLongPollingWait`, `ssOnlineUnstableLongPollingWait`, `ssDisconnecting`.

**Команды:** `ssCmdConnect/Disconnect`, `ssCmdPause/PauseRelease`, `ssCmdConnectingPingAck/Fault/Timeout`, `ssCmdDataReceived`, `ssCmdLongPolling*`, `ssCmdConnectionStable/Unstable`, `ssCmdQueueEmpty`.

**Метод `FSM_Process`** отрабатывает переходы, запускает и останавливает long‑polling, инициирует загрузки эндпоинтов и вызывает коллбеки.

#### HTTP‑клиенты подробно
- **`FHTTP`** — REST: очередь запросов, единый обработчик `HTTP_RestAPI`.
- **`FHTTPEvents`** — Event API: long‑polling; обработчики разрывов/ошибок, автоперезапуск по таймеру.
- Заголовок `X-API-Key` добавляется в `HttpAddHeader` при `OnBeginProcessing` каждого запроса.

#### JSON‑дерево и указатели подробно
- **Инициализация:** `CreateDefaultRoot` (+ `GetDefaultRootStr`).
- **Запись веток:** `SetJsonNodeAtPath` (с предварительным `FindAndCreatePathInJsonTree`).
- **Обновление указателей:** `UpdateJsonPointer` / `UpdateAllJsonPointers` (+ `UpdateJsonPointersFromTree`).
- **Высылка уведомлений:** `JSONTreeNewDataFromNetwork` → `NotifyTreeChanged`.

#### Инициализационная загрузка и эндпоинты
- **Базовые ресурсы:** массив `SyncthingEndpointsBasic` (config, system/connections, stats/device, stats/folder, system/status, system/version).
- **Загрузка:** `LoadAllBasicEndpoints` / `LoadEndpoint(Id)`.
- **Таблица соответствий:** `GetEndpointURI`, обратное сопоставление `GetEndpointIdByURI` (включая под‑ресурсы `.../@`).

#### Long‑polling событий подробно
- **Запуск/стоп/перезапуск:** `StartLongPolling`, `StopLongPolling` (+ таймер‑интервал).
- **Обработка:** `HTTP_EventAPI` → `HandleIncommingDataFromEventAPI` → `ProcessEvent` → дозагрузка нужных ресурсов.
- **Интеграция фрагментов в дерево:** зарезервирована `IntegrateEvent` (пока пустышка — логика сведена к дозагрузкам).

#### Публичный API
- **Настройки подключения:** `SetEndpoint`, `SetAPIKey`, таймауты (`ConnectTimeout`, `IOTimeout`, `ConnectingTimeout`, период перезапуска long‑polling).
- **Управление соединением:** `Connect`, `Disconnect`, `Pause`, `PauseRelease`, синхронный `Ping()`.
- **Состояние:** `State`, `IsOnline`, `Command`, данные: `TreeRoot` и типизированные указатели (`config_*`, `stats_*`).
- **Коллбеки:** `OnBeforeConnect/OnConnected/OnConnectError/OnBeforeDisconnect/OnDisconnectedByUser/OnHardDisconnect`, события long‑polling, дерево, смена состояния.

#### Обработка ошибок и восстановление
- **Таймауты соединения/ввода‑вывода,** авто‑ретраи в `AsyncHTTP`.
- **Коды псевдо‑ошибок** для некоторых ситуаций (например, разрыв keep‑alive) и повторная попытка.
- **При ошибках long‑polling** → состояние `ssOnlineUnstable`, далее восстановление по таймеру/событию.

#### Потокобезопасность
- **Сетевые коллбеки** приходят через механизм очереди/worker → `Synchronize`.
- **Перед модификацией дерева** вызывается `OnBeforeTreeModify`, после — `OnAfterTreeModify`.
- **UI бережно читает дерево** под `LockJSONTree`.

#### Расширяемость и заметки
- **Простое сопоставление эндпоинтов** позволяет добавлять новые ресурсы без изменения основной логики (через `GetEndpointCallback`/`GetEndpointURI`).
- **Возможна интеграция инкрементального применения фрагментов событий в дерево** (реализовать в `IntegrateEvent`).

-----------------------------------------
### Потоки и модель данных
- Единственный источник правды — `TSyncthingAPI.FTreeRoot`. Любые обновления приходят из сетевых коллбеков.
- Перед модификацией вызывается `OnBeforeTreeModify`, после — `OnAfterTreeModify`.
- `uModuleMain` использует `TCriticalSection` (`LockJSONTree`) для синхронизации отрисовки/чтения.

-----------------------------------------
### Сетевая модель
- REST: инициализационная загрузка «базовых» эндпоинтов, далее точечные дозагрузки по событиям.
- Events: long‑polling `/rest/events?since=<id>&limit=10&timeout=60` с автоперезапуском по таймеру или при сбоях.
- Заголовок `X-API-Key` добавляется в `OnBeginProcessing` каждого запроса.

-----------------------------------------
### UI‑детали
- Иконки устройств: 0=offline, 1=online, 2=paused. Онлайновость проверяется по `system.connections`.
- Подсказка трея — список онлайновых устройств (до N), «(local)» помечается по `IsLocalIP`.
- Индикатор круга меняет цвет в зависимости от `TSyncthingFSM_State`.

-----------------------------------------
### Известные WIP/задачи
- `uModuleCore` - устаревший модуль. Функционал будет перенесен. Еще не переперенос логики запуска/остановки Syncthing.
- `uModuleCore` не задействован в новой схеме.
- `uFormJsonView` содержит тестовые методы (mnTestConnect/mnTestJson).
- Не реализованы действия «Open Web UI» и часть обработчиков.

-----------------------------------------
### Сборка/запуск (кратко)
- Lazarus/FPC, открыть `SyncthingLazurite.lpi`, собрать.
- В настройках приложения указать путь к `syncthing.exe`, путь к каталогу с `config.xml` и/или API‑ключ.
- Подключиться и наблюдать обновления.


