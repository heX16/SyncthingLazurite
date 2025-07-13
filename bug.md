

Console:
Py Python 3.8.10 (tags/v3.8.10:3d8993a, May  3 2021, 11:48:03) [MSC v.1928 64 bit (AMD64)]
Py <syncthing.Syncthing object at 0x000000000C2F0A90>
Py Traceback (most recent call last):
Py   File "D:\heXor\App\INet_File\syncthing\Lib\urllib3\connection.py", line 174, in _new_conn
Py     conn = connection.create_connection(
Py   File "D:\heXor\App\INet_File\syncthing\Lib\urllib3\util\connection.py", line 95, in create_connection
Py     raise err
Py   File "D:\heXor\App\INet_File\syncthing\Lib\urllib3\util\connection.py", line 85, in create_connection
Py     sock.connect(sa)
Py ConnectionRefusedError: [WinError 10061] No connection could be made because the target machine actively refused it
Py
Py During handling of the above exception, another exception occurred:
Py
Py Traceback (most recent call last):
Py   File "D:\heXor\App\INet_File\syncthing\Lib\urllib3\connectionpool.py", line 703, in urlopen
Py     httplib_response = self._make_request(
Py   File "D:\heXor\App\INet_File\syncthing\Lib\urllib3\connectionpool.py", line 398, in _make_request
Py     conn.request(method, url, **httplib_request_kw)
Py   File "D:\heXor\App\INet_File\syncthing\Lib\urllib3\connection.py", line 239, in request
Py     super(HTTPConnection, self).request(method, url, body=body, headers=headers)
Py   File "http\client.py", line 1252, in request
Py   File "http\client.py", line 1298, in _send_request
Py   File "http\client.py", line 1247, in endheaders
Py   File "http\client.py", line 1007, in _send_output
Py   File "http\client.py", line 947, in send
Py   File "D:\heXor\App\INet_File\syncthing\Lib\urllib3\connection.py", line 205, in connect
Py     conn = self._new_conn()
Py   File "D:\heXor\App\INet_File\syncthing\Lib\urllib3\connection.py", line 186, in _new_conn
Py     raise NewConnectionError(
Py urllib3.exceptions.NewConnectionError: <urllib3.connection.HTTPConnection object at 0x000000000CDDFAC0>: Failed to establish a new connection: [WinError 10061] No connection could be made because the target machine actively refused it
Py
Py During handling of the above exception, another exception occurred:
Py
Py Traceback (most recent call last):
Py   File "D:\heXor\App\INet_File\syncthing\Lib\requests\adapters.py", line 489, in send
Py     resp = conn.urlopen(
Py   File "D:\heXor\App\INet_File\syncthing\Lib\urllib3\connectionpool.py", line 787, in urlopen
Py     retries = retries.increment(
Py   File "D:\heXor\App\INet_File\syncthing\Lib\urllib3\util\retry.py", line 592, in increment
Py     raise MaxRetryError(_pool, url, error or ResponseError(cause))
Py urllib3.exceptions.MaxRetryError: HTTPConnectionPool(host='localhost', port=8384): Max retries exceeded with url: /rest/system/version (Caused by NewConnectionError('<urllib3.connection.HTTPConnection object at 0x000000000CDDFAC0>: Failed to establish a new
Py  connection: [WinError 10061] No connection could be made because the target machine actively refused it'))
Py
Py During handling of the above exception, another exception occurred:
Py
Py Traceback (most recent call last):
Py   File "D:\heXor\App\INet_File\syncthing\Lib\syncthing\__init__.py", line 193, in _request
Py     resp = requests.request(
Py   File "D:\heXor\App\INet_File\syncthing\Lib\requests\api.py", line 59, in request
Py     return session.request(method=method, url=url, **kwargs)
Py   File "D:\heXor\App\INet_File\syncthing\Lib\requests\sessions.py", line 587, in request
Py     resp = self.send(prep, **send_kwargs)
Py   File "D:\heXor\App\INet_File\syncthing\Lib\requests\sessions.py", line 701, in send
Py     r = adapter.send(request, **kwargs)
Py   File "D:\heXor\App\INet_File\syncthing\Lib\requests\adapters.py", line 565, in send
Py     raise ConnectionError(e, request=request)
Py requests.exceptions.ConnectionError: HTTPConnectionPool(host='localhost', port=8384): Max retries exceeded with url: /rest/system/version (Caused by NewConnectionError('<urllib3.connection.HTTPConnection object at 0x000000000CDDFAC0>: Failed to establish a
Py new connection: [WinError 10061] No connection could be made because the target machine actively refused it'))
Py
Py The above exception was the direct cause of the following exception:
Py
Py Traceback (most recent call last):
Py   File "<string>", line 1, in <module>
Py   File "D:\heXor\App\INet_File\syncthing\Lib\syncthing\__init__.py", line 573, in version
Py     return self.get('version')
Py   File "D:\heXor\App\INet_File\syncthing\Lib\syncthing\__init__.py", line 163, in get
Py     return self._request('GET', endpoint, data, headers, params,
Py   File "D:\heXor\App\INet_File\syncthing\Lib\syncthing\__init__.py", line 209, in _request
Py     reraise('http request error', e)
Py   File "D:\heXor\App\INet_File\syncthing\Lib\syncthing\__init__.py", line 42, in reraise
Py     raise SyncthingError(msg) from exc
Py syncthing.SyncthingError: http request error

