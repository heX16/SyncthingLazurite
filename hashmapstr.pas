unit HashMapStr;

{$mode objfpc}{$H+}

interface

uses
  ghashmap,
  Classes, SysUtils;

type
  THashFuncString = class
    // return uniformly distributed i value in range <0,n-1> base only on arguments,
    // n will be always power of 2
    class function hash(a: string; n: SizeUInt): SizeUInt;

    // [when STL_INTERFACE_EXT is defined]
    // return the boolean test for equality of the two keys.  Typically this is operator=,
    // but it doesn't have to be (e.g. case-insensitive string comparison)
    class function equal(const AKey1, AKey2: string): Boolean;
  end;

implementation

uses
  CRC;

class function THashFuncString.hash(a: string; n: SizeUInt): SizeUInt;
begin
  Result := CRC.CRC32(n, @a[1], Length(a)) mod n;
end;

class function THashFuncString.equal(const AKey1, AKey2: string): Boolean;
begin
  Result := AKey1 = AKey2;
end;


end.

