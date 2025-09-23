unit uLogging;

{$mode objfpc}{$H+}

interface

// Prints a message to console only in DEBUG builds; does nothing otherwise
procedure DebugLog(const S: string);

implementation

{$IFDEF DEBUG}
uses
  SysUtils;

procedure DebugLog(const S: string);
var
  timeStr: string;
begin
  // Add time in HH:MM:SS format to the beginning of the message
  timeStr := FormatDateTime('hh:nn:ss', Now);
  WriteLn('[' + timeStr + '] ' + S);
end;

{$ELSE DEBUG}

procedure DebugLog(const S: string);
begin
end;

{$ENDIF DEBUG}

end.


