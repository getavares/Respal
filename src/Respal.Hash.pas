unit Respal.Hash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdHash, IdHMAC, IdGlobal;

type
  TRespalHashClass = class of TIdHMAC;
  TRespalHashMD5 = TIdHMACMD5;
  TRespalHashSHA1 = TIdHMACSHA1;
  TRespalHashSHA224 = TIdHMACSHA224;
  TRespalHashSHA256 = TIdHMACSHA256;
  TRespalHashSHA384 = TIdHMACSHA384;
  TRespalHashSHA512 = TIdHMACSHA512;

  { TRespalHash }

  TRespalHash = class
  private
  public
    class function Instance: TRespalHash;
    function Hash(ARespalHashClass: TRespalHashClass; AValue: String; AKey: String): TBytes;
  end;

implementation

var
  RespalHash: TRespalHash;

{ TRespalHash }

class function TRespalHash.Instance: TRespalHash;
begin
  if not Assigned(RespalHash) then
    RespalHash:=TRespalHash.Create;
  Result:=RespalHash;
end;

function TRespalHash.Hash(ARespalHashClass: TRespalHashClass; AValue: String;
  AKey: String): TBytes;
var
  HashObj: TIdHMAC;
begin
  HashObj:=ARespalHashClass.Create;
  try
    HashObj.Key:=AKey;
    Result:=TBytes(HashObj.HashValue(ToBytes(AValue, IndyTextEncoding_UTF8)));
  finally
    HashObj.Free;
  end;
end;

end.

