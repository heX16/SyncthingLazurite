unit syncthing_api_utils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, RegExpr;

// Parses ISO-like datetime string 'YYYY-MM-DDTHH:MM:SS[...]' to TDateTime
// Returns true on success and stores result to 'dt'
function JsonStrToDateTime(Str: AnsiString; out dt: TDateTime): boolean;

function IsLocalIP(IP: string): boolean;


implementation


function IsLocalIP(IP: string): boolean;
var
  B1, B2: byte;
  Parts: array[0..3] of string;
  PartCount: integer;
  i: integer;
  Part: string;
  PartValue: integer;
begin
  Result := false;
  IP := LowerCase(Trim(IP));

  if IP = '' then
    Exit;

  // Check if it looks like IPv6 (contains colon)
  if Pos(':', IP) > 0 then
  begin
    // Simple IPv6 checks

    if LeftStr(IP, 1) = '[' then
      Delete(IP, 1, 1);

    // Check for ::1 (localhost)
    if (IP = '::1') then
      Exit(true);

    // Check for fe80:: (link-local)
    if (Length(IP) >= 5) and (LeftStr(IP, 5) = 'fe80:') then
      Exit(true);

    // Check for ULA (fc00:: and fd00::) (unique local address)
    if (Length(IP) >= 2) and
       ((LeftStr(IP, 2) = 'fc') or (LeftStr(IP, 2) = 'fd')) then
      Exit(true);

    // For other IPv6 addresses, assume they are not local
    Exit(false);
  end;

  // Parse IPv4 manually
  PartCount := 0;
  Part := '';

  for i := 1 to Length(IP) do
  begin
    if IP[i] = '.' then
    begin
      if (PartCount >= 4) or (Part = '') then
        Exit(false); // Invalid format

      if not TryStrToInt(Part, PartValue) or (PartValue < 0) or (PartValue > 255) then
        Exit(false); // Invalid number

      Parts[PartCount] := Part;
      Inc(PartCount);
      Part := '';
    end
    else if IP[i] in ['0'..'9'] then
    begin
      Part := Part + IP[i];
    end
    else
    begin
      Exit(false); // Invalid character
    end;
  end;

  // Add the last part
  if (PartCount <> 3) or (Part = '') then
    Exit(false); // Should have exactly 4 parts

  if not TryStrToInt(Part, PartValue) or (PartValue < 0) or (PartValue > 255) then
    Exit(false); // Invalid number

  Parts[3] := Part;

  // Convert to bytes
  B1 := StrToInt(Parts[0]);
  B2 := StrToInt(Parts[1]);
  // B3 := StrToInt(Parts[2]);
  // B4 := StrToInt(Parts[3]);

  // Check IPv4 private/local ranges
  // 127.0.0.0/8 (localhost)
  if B1 = 127 then
    Exit(true);

  // 10.0.0.0/8 (private)
  if B1 = 10 then
    Exit(true);

  // 172.16.0.0/12 (private)
  if (B1 = 172) and (B2 >= 16) and (B2 <= 31) then
    Exit(true);

  // 192.168.0.0/16 (private)
  if (B1 = 192) and (B2 = 168) then
    Exit(true);

  // 169.254.0.0/16 (link-local)
  if (B1 = 169) and (B2 = 254) then
    Exit(true);

  Result := false;
end;



var
  // Singleton instance for compiled regular expression
  RegexDateTimeSingleton: TRegExpr = nil;

function GetRegexDateTime: TRegExpr;
begin
  if RegexDateTimeSingleton = nil then
  begin
    // Create once and reuse (singleton)
    RegexDateTimeSingleton := TRegExpr.Create('(\d+)-(\d+)-(\d+)T(\d+):(\d+):(\d+).*');
  end;
  Result := RegexDateTimeSingleton;
end;

function JsonStrToDateTime(Str: AnsiString; out dt: TDateTime): boolean;
var
  re: TRegExpr;
begin
  Result := false;
  re := GetRegexDateTime();
  if re.Exec(Str) then
  begin
    Result := TryEncodeDateTime(
      StrToInt(re.Match[1]),
      StrToInt(re.Match[2]),
      StrToInt(re.Match[3]),
      StrToInt(re.Match[4]),
      StrToInt(re.Match[5]),
      StrToInt(re.Match[6]),
      0,
      dt
    );
  end;
end;

finalization
  if RegexDateTimeSingleton <> nil then
    RegexDateTimeSingleton.Free;

end.


