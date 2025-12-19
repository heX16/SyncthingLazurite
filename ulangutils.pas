unit uLangUtils;

interface

uses
  Translations,
  gettext,
  Classes, SysUtils {add additional units that may be needed by your code here}
  {$IFDEF Windows}
  , Windows
  {$ELSE}
  , Unix
    {$IFDEF LCLCarbon}
  , MacOSAll
    {$ENDIF}
  {$ENDIF}
  ;

/// <summary>
///   Extracts cLanguageName and cLanguageNameEng values from PO file content.
///   Returns True if both values were found; outputs empty strings otherwise.
/// </summary>
function ExtractLanguageNames(const AContent: string; out LangName, LangNameEng: string): Boolean;

/// <summary>
///   Reads a small head of a UTF-8 text file and returns it as string.
///   Two mutually exclusive modes:
///     - If ALinesCount > 0: read that many lines (UTF-8, BOM-aware).
///     - Else: read up to AMaxBytes bytes, trim incomplete trailing UTF-8 and skip BOM.
///   Returns an empty string on failure.
/// </summary>
function ReadFirstBytesUtf8Safe(const AFileName: string; const AMaxBytes: Integer = 256; const ALinesCount: Integer = 0): string;


implementation

uses
  streamex;

function ExtractLanguageNames(const AContent: string; out LangName, LangNameEng: string): Boolean;
var
  Lines: TStringList;
  I: Integer;
  Line, CurrentId, Value: string;
begin
  LangName := '';
  LangNameEng := '';
  CurrentId := '';

  Lines := TStringList.Create;
  try
    Lines.Text := AContent;
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);

      if (Pos('msgid "', Line) = 1) then
      begin
        CurrentId := Copy(Line, 8, Length(Line) - 8); // extract text between quotes
        Continue;
      end;

      if (CurrentId <> '') and (Pos('msgstr "', Line) = 1) then
      begin
        Value := Copy(Line, 9, Length(Line) - 9); // extract text between quotes

        if CurrentId = 'cLanguageName' then
          LangName := Value
        else if CurrentId = 'cLanguageNameEng' then
          LangNameEng := Value;

        CurrentId := '';

        if (LangName <> '') and (LangNameEng <> '') then
          Break;
      end;
    end;
  finally
    Lines.Free;
  end;

  Result := (LangName <> '') and (LangNameEng <> '');
end;

function ReadFirstBytesUtf8Safe(const AFileName: string; const AMaxBytes: Integer = 256; const ALinesCount: Integer = 0): string;
var
  FS: TFileStream;
  Buf: TBytes;
  ReadCount: Integer;
  ValidCount: Integer;
  StartIdx: Integer;
  Reader: TStreamReader;
  LinesRead: Integer;
  Line: string;

  function IsUtf8Cont(const B: Byte): Boolean; inline;
  begin
    Result := (B and $C0) = $80;
  end;

  function Utf8SafeLength(const Data: TBytes; const StartPos, Count: Integer): Integer;
  var
    I, ContCount, ExpectedLen: Integer;
    StartByte: Byte;
  begin
    if Count = 0 then
      Exit(0);

    // Find start byte of the last UTF-8 sequence
    I := StartPos + Count - 1;
    while (I >= StartPos) and IsUtf8Cont(Data[I]) do
      Dec(I);

    // All bytes are continuation — treat as fully valid
    if I < StartPos then
      Exit(Count);

    ContCount := (StartPos + Count - 1) - I; // number of continuation bytes at the end
    StartByte := Data[I];

    if (StartByte and $80) = 0 then
      ExpectedLen := 1
    else if (StartByte and $E0) = $C0 then
      ExpectedLen := 2
    else if (StartByte and $F0) = $E0 then
      ExpectedLen := 3
    else if (StartByte and $F8) = $F0 then
      ExpectedLen := 4
    else
      ExpectedLen := 1; // invalid lead byte, best effort

    // If the trailing sequence is incomplete, drop it
    if (ContCount + 1) < ExpectedLen then
      Result := I - StartPos
    else
      Result := Count;
  end;

begin
  Result := '';
  if (AMaxBytes <= 0) and (ALinesCount <= 0) then
    Exit;

  if ALinesCount > 0 then
  begin
    // Branch 1: read by lines (priority)
    FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      Reader := TStreamReader.Create(FS);
      try
        LinesRead := 0;
        Result := '';
        while (LinesRead < ALinesCount) and (not Reader.Eof) do
        begin
          Line := Reader.ReadLine;
          if Result <> '' then
            Result := Result + LineEnding;
          Result := Result + Line;
          Inc(LinesRead);
        end;
      finally
        Reader.Free; // does not own FS in FPC
      end;
    finally
      FS.Free;
    end;
  end
  else
  begin
    // Branch 2: read by bytes (fallback)
    FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Buf, AMaxBytes);
      ReadCount := FS.Read(Buf[0], AMaxBytes);
      if ReadCount <= 0 then
        Exit;

      // Skip UTF-8 BOM if present
      StartIdx := 0;
      if (ReadCount >= 3) and (Buf[0] = $EF) and (Buf[1] = $BB) and (Buf[2] = $BF) then
        StartIdx := 3;

      ValidCount := Utf8SafeLength(Buf, StartIdx, ReadCount - StartIdx);
      if ValidCount > 0 then
        SetString(Result, PAnsiChar(@Buf[StartIdx]), ValidCount);
    finally
      FS.Free;
    end;
  end;
end;

end.
