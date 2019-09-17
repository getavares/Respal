unit Respal.Response;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdCustomHTTPServer, fpjson;

type
  TRespalResponse = class;

  { TRespalResponseBase }

  TRespalResponseBase = class
  private
    FResponse: TRespalResponse;
  protected
    property Response: TRespalResponse read FResponse;
  public
    constructor Create(AResponse: TRespalResponse); virtual;
    function Code(AValue: Integer): TRespalResponseBase; virtual;
    function ContentType(AValue: String): TRespalResponseBase; virtual;
    function Content(AValue: String): TRespalResponseBase; virtual;
    function Charset(AValue: String): TRespalResponseBase; virtual;
    function JSON(AValue: TJSONObject): TRespalResponseBase; virtual;
  end;

  { TRespalResponseSuccess }

  TRespalResponseSuccess = class(TRespalResponseBase)
  public
    function Code(AValue: Integer): TRespalResponseSuccess; reintroduce;
    function ContentType(AValue: String): TRespalResponseSuccess; reintroduce;
    function Content(AValue: String): TRespalResponseSuccess; reintroduce;
    function Charset(AValue: String): TRespalResponseSuccess; reintroduce;
    function JSON(AValue: TJSONObject): TRespalResponseSuccess; virtual;
    function Ok: TRespalResponseSuccess;
    function Created: TRespalResponseSuccess;
    function Accepted: TRespalResponseSuccess;
  end;

  { TRespalResponseClientError }

  TRespalResponseClientError = class(TRespalResponseBase)
  public
    function Code(AValue: Integer): TRespalResponseClientError; reintroduce;
    function ContentType(AValue: String): TRespalResponseClientError; reintroduce;
    function Content(AValue: String): TRespalResponseClientError; reintroduce;
    function Charset(AValue: String): TRespalResponseClientError; reintroduce;
    function JSON(AValue: TJSONObject): TRespalResponseClientError; virtual;
    function BadRequest: TRespalResponseClientError;
    function Unauthorized: TRespalResponseClientError;
    function Forbidden: TRespalResponseClientError;
    function NotFound: TRespalResponseClientError;
    function MethodNotAllowed: TRespalResponseClientError;
    function TooManyRequests: TRespalResponseClientError;
  end;

  { TRespalResponseServerError }

  TRespalResponseServerError = class(TRespalResponseBase)
  public
    function Code(AValue: Integer): TRespalResponseServerError; reintroduce;
    function ContentType(AValue: String): TRespalResponseServerError; reintroduce;
    function Content(AValue: String): TRespalResponseServerError; reintroduce;
    function Charset(AValue: String): TRespalResponseServerError; reintroduce;
    function JSON(AValue: TJSONObject): TRespalResponseServerError; virtual;
    function InternalServerError: TRespalResponseServerError;
    function ServiceUnavailable: TRespalResponseServerError;
  end;

  { TRespalResponse }

  TRespalResponse = class
    FResponseInfo: TIdHTTPResponseInfo;
  private
    FClientError: TRespalResponseClientError;
    FServerError: TRespalResponseServerError;
    FSuccess: TRespalResponseSuccess;
    function GetHeader(AName: String): String;
    procedure SetHeader(AName: String; AValue: String);
  public
    constructor Create(AResponseInfo: TIdHTTPResponseInfo); virtual;
    destructor Destroy; override;
    procedure Respond(AResponseNo: Word; AContent: String;
      ACharset: String = 'utf-8'; AContentType: String = 'text/html');
    property Headers[AName: String]: String read GetHeader write SetHeader;
    property Success: TRespalResponseSuccess read FSuccess;
    property ClientError: TRespalResponseClientError read FClientError;
    property ServerError: TRespalResponseServerError read FServerError;
  end;

implementation

{ TRespalResponseServerError }

function TRespalResponseServerError.Code(AValue: Integer
  ): TRespalResponseServerError;
begin
Result:=TRespalResponseServerError(inherited Code(AValue));
end;

function TRespalResponseServerError.ContentType(AValue: String
  ): TRespalResponseServerError;
begin
Result:=TRespalResponseServerError(inherited ContentType(AValue));
end;

function TRespalResponseServerError.Content(AValue: String
  ): TRespalResponseServerError;
begin
Result:=TRespalResponseServerError(inherited Content(AValue));
end;

function TRespalResponseServerError.Charset(AValue: String
  ): TRespalResponseServerError;
begin
Result:=TRespalResponseServerError(inherited Charset(AValue));
end;

function TRespalResponseServerError.JSON(AValue: TJSONObject
  ): TRespalResponseServerError;
begin
Result:=TRespalResponseServerError(inherited JSON(AValue));
end;

function TRespalResponseServerError.InternalServerError: TRespalResponseServerError;
begin
Result:=Code(500).Content('Internal Server Error').Charset('text/html');
end;

function TRespalResponseServerError.ServiceUnavailable: TRespalResponseServerError;
begin
Result:=Code(503).Content('Service Unavailable').Charset('text/html');
end;

{ TRespalResponseClientError }

function TRespalResponseClientError.Code(AValue: Integer
  ): TRespalResponseClientError;
begin
Result:=TRespalResponseClientError(inherited Code(AValue));
end;

function TRespalResponseClientError.ContentType(AValue: String
  ): TRespalResponseClientError;
begin
Result:=TRespalResponseClientError(inherited ContentType(AValue));
end;

function TRespalResponseClientError.Content(AValue: String
  ): TRespalResponseClientError;
begin
Result:=TRespalResponseClientError(inherited Content(AValue));
end;

function TRespalResponseClientError.Charset(AValue: String
  ): TRespalResponseClientError;
begin
Result:=TRespalResponseClientError(inherited Charset(AValue));
end;

function TRespalResponseClientError.JSON(AValue: TJSONObject
  ): TRespalResponseClientError;
begin
Result:=TRespalResponseClientError(inherited JSON(AValue));
end;

function TRespalResponseClientError.BadRequest: TRespalResponseClientError;
begin
Result:=Code(400).Content('Bad Request').ContentType('text/html');
end;

function TRespalResponseClientError.Unauthorized: TRespalResponseClientError;
begin
Result:=Code(401).Content('Unauthorized').ContentType('text/html');
end;

function TRespalResponseClientError.Forbidden: TRespalResponseClientError;
begin
Result:=Code(403).Content('Forbidden').ContentType('text/html');
end;

function TRespalResponseClientError.NotFound: TRespalResponseClientError;
begin
Result:=Code(404).Content('Not Found').ContentType('text/html');
end;

function TRespalResponseClientError.MethodNotAllowed: TRespalResponseClientError;
begin
Result:=Code(405).Content('Method Not Allowed').ContentType('text/html');
end;

function TRespalResponseClientError.TooManyRequests: TRespalResponseClientError;
begin
Result:=Code(429).Content('Too Many Requests').ContentType('text/html');
end;

{ TRespalResponseSuccess }

function TRespalResponseSuccess.Code(AValue: Integer): TRespalResponseSuccess;
begin
Result:=TRespalResponseSuccess(inherited Code(AValue));
end;

function TRespalResponseSuccess.ContentType(AValue: String
  ): TRespalResponseSuccess;
begin
Result:=TRespalResponseSuccess(inherited ContentType(AValue));
end;

function TRespalResponseSuccess.Content(AValue: String): TRespalResponseSuccess;
begin
Result:=TRespalResponseSuccess(inherited Content(AValue));
end;

function TRespalResponseSuccess.Charset(AValue: String): TRespalResponseSuccess;
begin
Result:=TRespalResponseSuccess(inherited Charset(AValue));
end;

function TRespalResponseSuccess.JSON(AValue: TJSONObject): TRespalResponseSuccess;
begin
Result:=TRespalResponseSuccess(inherited JSON(AValue));
end;

function TRespalResponseSuccess.Ok: TRespalResponseSuccess;
begin
Result:=Code(200).Content('Ok').ContentType('text/html');
end;

function TRespalResponseSuccess.Created: TRespalResponseSuccess;
begin
Result:=Code(201).Content('Created').ContentType('text/html');
end;

function TRespalResponseSuccess.Accepted: TRespalResponseSuccess;
begin
Result:=Code(202).Content('Accepted').ContentType('text/html');
end;

{ TRespalResponse }

function TRespalResponse.GetHeader(AName: String): String;
begin
Result:=FResponseInfo.RawHeaders.Values[AName];
end;

procedure TRespalResponse.SetHeader(AName: String; AValue: String);
begin
FResponseInfo.RawHeaders.Values[AName]:=AValue;
end;

constructor TRespalResponse.Create(AResponseInfo: TIdHTTPResponseInfo);
begin
FResponseInfo:=AResponseInfo;
FSuccess:=TRespalResponseSuccess.Create(Self);
FClientError:=TRespalResponseClientError.Create(Self);
FServerError:=TRespalResponseServerError.Create(Self);
end;

destructor TRespalResponse.Destroy;
begin
inherited Destroy;
end;

procedure TRespalResponse.Respond(AResponseNo: Word; AContent: String;
  ACharset: String; AContentType: String);
begin
FResponseInfo.ResponseNo:=AResponseNo;
FResponseInfo.ContentText:=AContent;
FResponseInfo.CharSet:=ACharset;
FResponseInfo.ContentType:=AContentType;
end;

{ TRespalResponseBase }

constructor TRespalResponseBase.Create(AResponse: TRespalResponse);
begin
FResponse:=AResponse;
end;

function TRespalResponseBase.Code(AValue: Integer): TRespalResponseBase;
begin
FResponse.FResponseInfo.ResponseNo:=AValue;
Result:=Self;
end;

function TRespalResponseBase.ContentType(AValue: String): TRespalResponseBase;
begin
FResponse.FResponseInfo.ContentType:=AValue;
Result:=Self;
end;

function TRespalResponseBase.Content(AValue: String): TRespalResponseBase;
begin
FResponse.FResponseInfo.ContentText:=AValue;
Result:=Self;
end;

function TRespalResponseBase.Charset(AValue: String): TRespalResponseBase;
begin
FResponse.FResponseInfo.CharSet:=AValue;
Result:=Self;
end;

function TRespalResponseBase.JSON(AValue: TJSONObject): TRespalResponseBase;
begin
Result:=ContentType('application/json').Content(AValue.ToString);
end;

end.

