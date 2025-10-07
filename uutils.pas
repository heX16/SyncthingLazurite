unit uUtils;

{$mode ObjFPC}{$H+}

interface

uses
  AsyncHttp,
  fpjson,
  Classes;

function StripAfterChar(const S: string; const Delim: Char; ReverseFind: boolean = false): string;

function HttpRequestToJson(Request: THttpRequest; out Json: TJSONData): boolean;

implementation

uses
  LazUTF8,
  StrUtils,
  jsonscanner,
  jsonparser;

function StripAfterChar(const S: string; const Delim: Char; ReverseFind: boolean
  ): string;
var
  P: SizeInt;
begin
  // Remove substring starting from the first delimiter occurrence
  if not ReverseFind then
    P := Pos(Delim, S) else
    P := RPos(Delim, S);
  if P > 0 then
    Result := Copy(S, 1, P - 1)
  else
    Result := S;
end;

function HttpRequestToJson(Request: THttpRequest; out Json: TJSONData): boolean;
var
  JN: TJSONParser;
  JData: TJSONData;
  MS: TMemoryStream;
begin
  Result := false;
  Json := nil;
  if (Request = nil) then Exit;
  if Request.Status = 499 then Exit;

  if (Request.Response <> nil) and (Request.Response.Size > 0) then
  begin
    JN := nil;
    MS := TMemoryStream.Create;
    try
      // Copy response into a local stream to decouple from Request.Response
      Request.Response.Position := 0;
      MS.CopyFrom(Request.Response, Request.Response.Size);
      MS.Position := 0;

      JN := TJSONParser.Create(MS, [joUTF8]);
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
    finally
      if JN <> nil then
        JN.Free();
      MS.Free;
    end;
  end;
end;

end.

