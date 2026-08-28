# Поток Connect, состояние действий и известные проблемы UI

Заметки сессии (2026-08-29). Для разработчиков — **не рассматривайте этот документ как спецификацию исправления**; он описывает только текущее поведение и проблемы.

---

## Цепочка вызовов Connect (кратко)

**Точка входа:** `TModuleMain.actConnectExecute` (`umodulemain.pas`)

1. Читает порт из `frmOptions.edPortNumber` через `GetPortFromOptions()` (по умолчанию 8384).
2. Читает API-ключ из `frmOptions.edAPIKey` (если форма существует).
3. `FSyncthingAPI.SetEndpoint` → `BuildServerURL` → например `http://127.0.0.1:8384/`
4. `FSyncthingAPI.SetAPIKey` (если ключ непустой)
5. `FSyncthingAPI.Connect()`

`FSyncthingAPI` — это `TSyncthingManager` (наследует `TSyncthingAPI`). 

`Connect` в менеджере **не** переопределён.


### `Connect()` → FSM

```
Connect
  → Command := ssCmdConnect
  → FSM_Process()
```

**Синхронная часть (поток UI):**

| Шаг | Состояние FSM | Что происходит |
|-----|---------------|----------------|
| 1 | `ssOffline` → `ssConnectingInitAndPing` | Опционально `FOnBeforeConnect` (в главном модуле не используется) |
| 2 | `ssConnectingInitAndPing` | `BuildServerURL`, `ConfigureHttpClient`, асинхронный `API_Get('system/ping')` |
| 3 | → `ssConnectingPingWait` | Включается таймер таймаута подключения (5 с) |

`actConnectExecute` возвращается здесь; остальное выполняется асинхронно.

**Асинхронная часть (HTTP-воркер → callback в главном потоке):**

- **Ping OK:** `HTTP_CheckOnline` → `ssCmdConnectingPingAck` → `ssConnectingWaitData` → `LoadAllBasicEndpoints()` (6 GET: config, connections, stats device/folder, status, version).
- Каждый REST-ответ: `HTTP_RestAPI` → обновление JSON-дерева → `ssCmdDataReceived` → FSM.
- Когда все базовые эндпоинты загружены → `ssOnlineLongPollingWait` → **`OnConnected`** (`Syn_OnConnected`, сейчас почти пустой).
- **Ping fail / таймаут 5 с:** → `ssOffline` → `OnConnectError` (`Syn_OnConnectError` → журнал событий).

**После «подключения»:**

- Таймер `FTimerLongPullingDoConnect` (500 мс) → `StartLongPolling` → `GET /rest/events?...&timeout=60`.
- Переход в `ssOnline` (`IsOnline = True`) — когда счётчик `FLongPollingWaitGoodBadCount >= 2` (см. раздел ниже; это **не** «2 ответа с событиями»).
- События → `Syn_OnEvent`; обновления REST-дерева → `Syn_OnTreeChanged`.

### Другие точки входа в тот же путь подключения

- `actConnectOrStartExecute` — если процесс уже запущен, вызывает `actConnectExecute`.
- `TSyncthingManager.ProcessStateChanged` — при переходе в `psRunning` автоматически вызывает `Connect`, если не в онлайне.

### Важная семантика

- `Connect` **не** блокирует UI.
- `OnConnected` срабатывает в `ssOnlineLongPollingWait`, но `IsOnline` всё ещё `False`, пока FSM не достигнет `ssOnline`, `ssOnlinePaused` или `ssOnlineUnstable`.

---

## Как включаются/отключаются действия (кнопки)

Два механизма обновляют действия панели инструментов/меню:

| Механизм | Триггер | Расположение |
|----------|---------|--------------|
| `Syn_OnStateChanged` | Каждый `SetState` FSM | `umodulemain.pas` |
| `UpdateActionsEnabled` | В конце `Syn_OnStateChanged`; инициализация приложения; `Mgr_OnProcessStateChanged` | `umodulemain.pas` |

### Правила `UpdateActionsEnabled`

Использует **`IsOnline`** (FSM API) и **`IsProcessRunning`** (процесс ОС):

| Действие | Включено, когда |
|----------|-----------------|
| `actConnect` | `not IsOnline` |
| `actDisconnect` | `IsOnline` |
| `actConnectOrStart` | `not IsProcessRunning` |
| `actDisconnectAndStop` | `IsProcessRunning` |
| `actStopAndExit` | `IsProcessRunning` |

`IsOnline` (`syncthing_api.pas`):

```pascal
Result := (State = ssOnline) or (State = ssOnlinePaused) or (State = ssOnlineUnstable);
```

Состояния подключения и `ssOnlineLongPollingWait` **не** считаются «онлайн» для этой проверки.

### Дополнительные правила `Syn_OnStateChanged`

В начале обработчика и `actConnect`, и `actDisconnect` устанавливаются в **включённое** состояние, затем `case NewState` корректирует некоторые из них и цвет индикатора статуса:

| Состояние FSM | Дополнительные изменения действий | Цвет статуса |
|---------------|-----------------------------------|--------------|
| `ssOffline` | `actDisconnect` := disabled | серый |
| `ssConnectingInitAndPing`, `ssConnectingPingWait`, `ssConnectingWaitData` | (нет в case) | жёлтый |
| `ssOnline`, `ssOnlineLongPollingWait` | `actConnect` := disabled | зелёный |
| `ssOnlinePaused` | (нет) | серебристый |
| `ssOnlineUnstable`, `ssOnlineUnstableLongPollingWait` | (нет) | красный |
| `ssDisconnecting` | (нет) | жёлтый |

Затем **`UpdateActionsEnabled` всегда вызывается** и перезаписывает `actConnect` / `actDisconnect` на основе `IsOnline`.

### Действия, не управляемые `UpdateActionsEnabled`

Примеры: `actStopSyncthing`, `actRestartApp`, `actSetStateRun`, `actSetStateStop` и общие действия UI (Options, About и т.д.). Некоторые по умолчанию имеют `Enabled = False` в `umodulemain.lfm`.

---

## Баг: мерцание кнопок действий во время Connect

### Симптом

При одном нажатии **Connect** кнопки **Connect** и **Disconnect** несколько раз подряд меняют доступность, хотя для пользователя это одна операция.

Наиболее заметно после успешного подключения: индикатор уже зелёный, данные загружены, но **Connect** на мгновение гаснет и снова включается, а **Disconnect** ещё долго неактивен — до перехода FSM в `ssOnline`.

### Корневая причина

**1. FSM генерирует много смен состояний подряд**

Один Connect запускает цепочку состояний, каждое из которых вызывает `SetState` → `Syn_OnStateChanged` → `UpdateActionsEnabled`:

```
ssOffline
  → ssConnectingInitAndPing
  → ssConnectingPingWait
  → ssConnectingWaitData
  → ssOnlineLongPollingWait
  → (позже) ssOnline
```

Каждый переход пересчитывает флаги доступности действий → видимое мерцание.

**2. Конфликтующая логика между `Syn_OnStateChanged` и `UpdateActionsEnabled`**

Для `ssOnlineLongPollingWait` в частности:

| Шаг | Что устанавливает `actConnect.Enabled` |
|-----|----------------------------------------|
| case в `Syn_OnStateChanged` | **False** (обрабатывает `ssOnlineLongPollingWait` как «достаточно онлайн, чтобы отключить Connect») |
| `UpdateActionsEnabled` сразу после | **True** (`IsOnline` всё ещё False, потому что `ssOnlineLongPollingWait` исключён из `IsOnline`) |

Таким образом, после успешной загрузки базовых данных Connect может **кратковременно отключиться и снова включиться**, пока long polling не стабилизируется и FSM не достигнет `ssOnline` — после чего Connect снова отключается, а Disconnect включается.

**3. Избыточное «включить оба» в начале `Syn_OnStateChanged`**

Строки 651–652 устанавливают и Connect, и Disconnect в enabled при **каждой** смене состояния, до case и до `UpdateActionsEnabled`. Это добавляет лишние переключения; итоговые значения всё равно приходят из `UpdateActionsEnabled` (кроме случаев, когда case и `UpdateActionsEnabled` расходятся, как выше).

### Ожидание vs факт (с точки зрения пользователя)

| Фаза | Ожидание пользователя | Текущее поведение |
|------|----------------------|-------------------|
| Подключение (`ssConnecting*`) | Connect отключён или без изменений; Disconnect отключён | Connect **включён**, Disconnect **отключён** (`UpdateActionsEnabled`) — приемлемо, но смены состояний всё равно вызывают много обновлений |
| Данные загружены, ожидание long poll (`ssOnlineLongPollingWait`) | Connect отключён; Disconnect включён (уже «подключено») | Connect **мерцает** (case отключает, затем `UpdateActionsEnabled` снова включает) |
| Стабильный онлайн (`ssOnline`) | Connect отключён; Disconnect включён | Корректно |

### Затронутый код (справка)

- `TModuleMain.Syn_OnStateChanged` — `umodulemain.pas`
- `TModuleMain.UpdateActionsEnabled` — `umodulemain.pas`
- `TSyncthingAPI.SetState` / `IsOnline` — `syncthing_api.pas`
- Путь connect в FSM — `TSyncthingAPI.FSM_Process`, `Connect` — `syncthing_api.pas`



## Архитектурная проблема: два независимых домена состояния для действий

### Что есть сейчас

Доступность действий GUI определяется **двумя несвязанными источниками состояния**:

| Домен | Источник | API | Управляет действиями |
|-------|----------|-----|----------------------|
| **Подключение к API** | FSM в `TSyncthingAPI` | `IsOnline`, `OnStateChanged` | `actConnect`, `actDisconnect` |
| **Процесс ОС** | Монитор процессов `TSyncthingManager` | `IsProcessRunning`, `OnProcessStateChanged` | `actConnectOrStart`, `actDisconnectAndStop`, `actStopAndExit` |

### Почему это проблема

**1. Нет единого «состояния приложения» для UI**

Пользователь видит один GUI Syncthing, но доступность действий вычисляется из двух слоёв, которые могут расходиться:

- Процесс **запущен**, API **офлайн** (Syncthing работает, подключение не удалось / ещё не выполнено).
- Процесс **не запущен**, API теоретически может быть «онлайн», если подключение к удалённому экземпляру (ещё не реализовано, но TODO для удалённого хоста есть).
- Автоподключение при `psRunning` vs ручной Connect — тот же путь API, разные действия процесса.

**2. Пересекающиеся концепции с разными именами**

| Ментальная модель пользователя | Разделение в коде |
|--------------------------------|-------------------|
| «Запустить Syncthing и подключиться» | `actConnectOrStart` (процесс) + затем авто/ручной `Connect` (API) |
| «Отключиться от API» | `actDisconnect` |
| «Остановить Syncthing» | `actDisconnectAndStop`, `actStopSyncthing`, `actStopAndExit` — несколько действий, разные комбинации процесса и API |

**3. `IsOnline` vs «достаточно подключено для UI»**

- `OnConnected` срабатывает в `ssOnlineLongPollingWait`.
- `IsOnline` становится true только в `ssOnline` (или вариантах paused/unstable).
- `Syn_OnStateChanged` обрабатывает `ssOnlineLongPollingWait` как онлайн для **отключения Connect**, но `IsOnline` — нет.

Три разных понятия «подключено» сосуществуют без задокументированного единого источника истины для действий.

**4. `Syn_OnStateChanged` дублирует и конфликтует с `UpdateActionsEnabled`**

Правила действий разнесены между:

- per-state `case` в `Syn_OnStateChanged` (частично, цвет + некоторые действия),
- централизованным `UpdateActionsEnabled` (API + процесс),

что приводит к багу мерцания и затрудняет ответ на вопрос «должен ли Connect быть включён в состоянии X?» без чтения обоих мест.

**5. Смена состояния процесса может обновлять действия без изменения состояния API**

`Mgr_OnProcessStateChanged` → `UpdateActionsEnabled` обновляет только действия, зависящие от процесса, но вызов происходит по событиям монитора процессов; если состояние процесса «дрожит» (`psRunning` ↔ `psUnknown`, см. исторические логи в `doc/bugs.md`), действия, связанные с процессом, могут мерцать независимо от connect FSM.

### Открытый вопрос проектирования (только постановка проблемы)

UI нуждается в **согласованной модели** того, когда каждое действие допустимо — вероятно, в одном производном представлении «что делает приложение» (idle / запуск процесса / подключение / подключено / нестабильно / остановка) — вместо склеивания API FSM и состояния процесса в двух обработчиках. **Этот документ не предписывает решение**; он фиксирует, что текущее разделение несогласованно и уже вызывает видимые баги.

---

## Проблема: таймер и счётчик long polling

**Вердикт:** `FTimerLongPullingDoConnect` (500 мс) и `FLongPollingWaitGoodBadCount` — **эвристика, близкая к костылю**, и явный источник проблем в будущем. Та же логика продублирована в `python/syncthing_api.py`.

### Как это устроено сейчас

В состояниях `ssOnlineLongPollingWait` / `ssOnlineUnstableLongPollingWait`:

1. Каждые **500 мс** таймер шлёт `ssCmdLongPollingConnect`.
2. FSM: если запрос `polling` **ещё не в очереди** → `StartLongPolling()`; если **уже есть** → `FLongPollingWaitGoodBadCount += 1`.
3. При `>= 2` → переход в `ssOnline`; при `<= -10` → `ssOffline`. Пороги помечены `TODO: create constant`.

**Важно:** счётчик растёт не от «успешного ответа `/rest/events` с событиями», а от того, что **HTTP-запрос polling ещё числится активным** в момент срабатывания таймера. На практике `ssOnline` наступает примерно через **1–1,5 с** после старта первого long poll, даже если событий ноль. Таймаут `timeout=60` на это не влияет.

### Почему это выглядит криво

| Наблюдение | Следствие |
|------------|-----------|
| Стабильность = «запрос в очереди два тика таймера» | Не проверяется реальный успех HTTP, наличие событий, качество канала |
| Magic numbers `2` и `-10` | Неочевидный смысл, нет именованных констант и документации |
| Таймер вместо явных переходов по callback HTTP | «Сторожевой» опрос очереди — типичный обход незавершённого async lifecycle |
| Два уровня «online»: `ssOnlineLongPollingWait` vs `ssOnline` | Лишний промежуточный статус; разрыв с `OnConnected` и `IsOnline` → мерцание кнопок |
| Из `ssOnline` успешный long poll **снова** переводит в `ssOnlineLongPollingWait` | «Стабильное» состояние само себя destabilize'ит |
| Четыре таймера в подсистеме connect/poll (5 с, 500 мс, ~65 с, 10 с) | Сложно рассуждать о порядке событий; риск гонок |

### Связь с уже известными симптомами

- **Мерцание кнопок** — зазор между `OnConnected` / зелёным индикатором (`ssOnlineLongPollingWait`) и `IsOnline` (`ssOnline`), который существует только из‑за этой схемы «почти online → проверка счётчиком → online».
- **Циклы в логах** (`doc/bugs.md`) — частые `ssCmdLongPollingConnect`, `LongPollingError`, `ForceRestart` в `ssOnlineLongPollingWait` указывают на хрупкость, а не на редкий edge case.

### Риски на будущее

- Любое изменение UI, actions или «когда мы считаем себя online», потребует помнить про **три** критерия (`OnConnected`, `IsOnline`, цвет в `Syn_OnStateChanged`) и про **неочевидный** счётчик.
- Рефакторинг `TAsyncHTTP` или таймингов может **сломать** переход в `ssOnline` (счётчик перестанет набираться) или, наоборот, переводить в online **слишком рано**.
- Подключение к **удалённому** Syncthing без локального процесса усилит путаницу: long polling и FSM станут единственным источником «живости», а текущая эвристика для этого слабая.

**Этот документ не предписывает решение** — только фиксирует, что таймер + счётчик — архитектурный долг и кандидат на пересмотр вместе с единой моделью состояния для UI.

---

## Связанные файлы

| Файл | Роль |
|------|------|
| `umodulemain.pas` | `actConnectExecute`, `Syn_OnStateChanged`, `UpdateActionsEnabled` |
| `syncthing_api.pas` | FSM, `Connect`, `IsOnline`, `SetState`, таймер/счётчик long polling |
| `python/syncthing_api.py` | Та же логика FSM и счётчика (порт) |
| `usyncthingmanager.pas` | Жизненный цикл процесса, автоподключение при `psRunning` |
| `umodulemain.lfm` | Определения действий и значения `Enabled` по умолчанию |
| `doc/bugs.md` | Исторические логи (дрожание состояния процесса, циклы FSM) |

---

## TODO в пути connect (контекст)

Из `actConnectExecute` и архитектурных заметок — не реализовано:

- Удалённый хост (не только `127.0.0.1`)
- TLS
- Автоопределение API-ключа из конфига Syncthing

Это может повлиять на будущее проектирование действий/состояний (например, подключение без локального процесса).
