unit uLogging;

{$mode objfpc}{$H+}

interface

// Prints a message to console only in DEBUG builds; does nothing otherwise
procedure DebugLog(const S: string);

implementation

{$IFDEF DEBUG}
procedure DebugLog(const S: string);
begin
  WriteLn(S);
end;
{$ELSE}
procedure DebugLog(const S: string);
begin
end;
{$ENDIF}

end.


