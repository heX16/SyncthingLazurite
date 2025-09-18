unit uUtils;

{$mode ObjFPC}{$H+}

interface

function StripAfterChar(const S: string; const Delim: Char; ReverseFind: boolean = false): string;

implementation

uses
  StrUtils;

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

end.

