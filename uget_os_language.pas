unit uget_os_language;

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

{platform-independent method to read the language of the user interface}
function GetOSLanguage: string;

implementation

// Examples of switching UI language at runtime (caller side):
//   SetDefaultLang(GetOSLanguage, IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'languages');
//   SetDefaultLang('ru', IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'languages');
// Requires units Forms and DefaultTranslator in the caller.

function GetOSLanguage: string;
var
  l, fbl: string;
  {$IFDEF LCLCarbon}
  theLocaleRef: CFLocaleRef;
  locale: CFStringRef;
  buffer: StringPtr;
  bufferSize: CFIndex;
  encoding: CFStringEncoding;
  success: boolean;
  {$ENDIF}
begin
  {$IFDEF LCLCarbon}
  theLocaleRef := CFLocaleCopyCurrent;
  locale := CFLocaleGetIdentifier(theLocaleRef);
  encoding := 0;
  bufferSize := 256;
  buffer := new(StringPtr);
  success := CFStringGetPascalString(locale, buffer, bufferSize, encoding);
  if success then
    l := string(buffer^)
  else
    l := '';
  fbl := Copy(l, 1, 2);
  dispose(buffer);
  {$ELSE}
  {$IFDEF LINUX}
  fbl := Copy(GetEnvironmentVariable('LC_CTYPE'), 1, 2);
    {$ELSE}
    GetLanguageIDs(l, fbl);
    //LCLGetLanguageIDs(l, fbl);
    {$ENDIF}
  {$ENDIF}
  Result := fbl;
end;

end.
