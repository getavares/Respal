unit Respal.Request;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdCustomHTTPServer, Respal.Uri;

type
  { TRespalRequest }

  TRespalRequest = class
  private
    FRequestInfo: TIdHTTPRequestInfo;
    FUri: TRespalUri;
    function GetHeaders(AName: String): String;
  public
    constructor Create(ARequestInfo: TIdHTTPRequestInfo); virtual;
    destructor Destroy; override;
    property Uri: TRespalUri read FUri;
    property Headers[AName: String]: String read GetHeaders;
  end;

implementation

{ TRespalRequest }

function TRespalRequest.GetHeaders(AName: String): String;
begin
Result:=FRequestInfo.RawHeaders.Values[AName];
end;

constructor TRespalRequest.Create(ARequestInfo: TIdHTTPRequestInfo);
begin
FRequestInfo:=ARequestInfo;
FUri:=TRespalUri.Create(FRequestInfo.URI);
end;

destructor TRespalRequest.Destroy;
begin
inherited Destroy;
end;

end.

