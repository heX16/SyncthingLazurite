# Техническое задание: Асинхронная HTTP библиотека

## Назначение
Создать библиотеку для выполнения неблокирующих HTTP-запросов с callback-based API, имитирующую современные async/await паттерны в рамках возможностей языка без встроенной поддержки асинхронности.

## Основные принципы

### 1. **Неблокирующие запросы**
- HTTP-запросы не должны блокировать основной поток выполнения
- Результат возвращается через callback-функции

### 2. **Thread-safe архитектура**
- Использование worker thread для выполнения сетевых операций
- Потокобезопасная очередь запросов
- Callback выполняется в контексте основного потока

### 3. **Автоматическое управление ресурсами**
- Объекты запросов создаются и уничтожаются автоматически
- Отсутствие memory leaks при асинхронных операциях

## Публичный API

### Основной класс: `AsyncHTTP`

```pascal
// Инициализация
constructor Create();
property ConnectTimeout: Integer;  // Таймаут соединения (мс)

// Основные методы запросов
procedure Get(url: string; callback: CallbackFunction; headers: string = '');
procedure Post(url: string; data: string; callback: CallbackFunction; headers: string = '');

// Универсальный метод
procedure HttpMethod(method: string; url: string; callback: CallbackFunction; 
                    headers: string = ''; data: string = '');

// Управление жизненным циклом
procedure Terminate();  // Корректное завершение работы
```

### Callback-функция

```pascal
type CallbackFunction = procedure(query: HttpQuery) of object;

// В callback доступны:
query.Status      // HTTP статус код
query.Response    // Поток с данными ответа
query.Connected   // Успешность соединения
```

### Глобальные события (опционально)

```pascal
property OnOpened: EventHandler;    // Запрос начат
property OnLoadDone: EventHandler;  // Запрос завершен успешно  
property OnError: EventHandler;     // Ошибка выполнения
```

### Отслеживание состояния операций

**workFlag** - указатель на boolean переменную клиента. Библиотека автоматически:
- Устанавливает `workFlag^ = true` при начале запроса
- Сбрасывает `workFlag^ = false` при завершении (успех или ошибка)

Позволяет клиенту отслеживать состояние операции без polling.

```pascal
var isLoading: boolean;
http.Get(url, @callback, '', @isLoading);
// isLoading автоматически управляется библиотекой
```

NOTE:
Это плохой паттерн.
Потомучто неопытные пользователи будут указывать локальную переменную (что будет приводить в повреждению стека).
А также есть другие очевидные минусы.
Я думаю более надежно и просто будет указывать строку (`OperationName: ansistring`) которая будет являться именем текущей операции.
И будет функция CheckOperation(OperationName) для проверки статуса этой операции.
CheckOperation возвращает следующие варианты: в очереди, в процессе, успешно завершенно (и результат еще не удален из памяти), в памяти нет опарации с таким именем (небыло или уже удалено).
Для хранения строк будет использовать HashMap (`Dict` в терминах Python).
Сами строки хранить не нужно - достаточно хранить их хеш `uint64`.
После вызова функции `CallbackFunction` будет удалено: внутренний объект который хранил всю информацию о функции, а также "имя операции" из HashMap.

## Поведение системы

### 1. **Жизненный цикл запроса**
1. Клиент вызывает `Get()` или `Post()`
2. Запрос добавляется в потокобезопасную очередь
3. Worker thread обрабатывает запрос
4. По завершении callback вызывается в основном потоке
5. Объект запроса автоматически уничтожается

### 2. **Обработка ошибок**
- Автоматические повторы при таймаутах
- Callback вызывается даже при ошибках
- Статус ошибки доступен через `query.Status`

### 3. **Thread Safety**
- Все методы можно вызывать из любого потока
- Callback гарантированно выполняется в UI потоке
- Корректная синхронизация доступа к очереди

## Пример использования

```pascal
// Инициализация
http := AsyncHTTP.Create();
http.ConnectTimeout := 1000;

// Простой GET запрос
http.Get('http://api.example.com/data', @OnDataReceived);

// POST с данными
http.Post('http://api.example.com/submit', jsonData, @OnSubmitComplete);

// Отслеживание состояния
var isLoading: boolean;
http.Get('http://slow-api.com', @OnResponse, '', @isLoading);

// Обработка ответа
procedure OnDataReceived(query: HttpQuery);
begin
  if query.Connected then
    ProcessResponse(query.Response)
  else
    ShowError('Request failed: ' + IntToStr(query.Status));
end;

// Завершение работы
http.Terminate();
```

## Ключевые особенности реализации

1. **Worker thread** обрабатывает очередь запросов
2. **Critical sections** для потокобезопасности
3. **Event objects** для синхронизации потоков
4. **Application message queue** для выполнения callback в UI потоке
5. **Automatic cleanup** через механизм отложенного уничтожения

Эта архитектура обеспечивает современный async-like API в языках без встроенной поддержки асинхронности.
