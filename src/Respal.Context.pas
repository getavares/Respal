unit Respal.Context;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdContext, Respal.Response, Respal.Request, Respal.Session;

type

  { TRespalUserContext }

  TRespalUserContext = class
  private
    FCustomData: TStringList;
    FLoggedSince: TDateTime;
    FRealm: String;
    FRoles: TStringList;
    FUserName: String;
    procedure SetCustomData(AValue: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
    property UserName: String read FUserName write FUserName;
    property Roles: TStringList read FRoles;
    property LoginDate: TDateTime read FLoggedSince write FLoggedSince;
    property CustomData: TStringList read FCustomData write SetCustomData;
    property Realm: String read FRealm write FRealm;
  end;

  TRespalUserSetStringValue = function: string;
  TRespalUserSetStringListValue = procedure(const AList: TStringList);
  TRespalUserSetDateTimeValue = function: TDateTime;

  { TRespalContext }

  TRespalContext = class
  private
    FContext: TIdContext;
    FRequest: TRespalRequest;
    FResponse: TRespalResponse;
    FSession: TRespalSessionContext;
    FUser: TRespalUserContext;
    function GetSession: TRespalSession;
  public
    constructor Create(AContext: TIdContext; ARequest: TRespalRequest;
      AResponse: TRespalResponse; ASession: TRespalSessionContext); virtual;
    destructor Destroy; override;
    function CreateSession: TRespalSession;
    property Session: TRespalSession read GetSession;
    property Request: TRespalRequest read FRequest;
    property Response: TRespalResponse read FResponse;
    property User: TRespalUserContext read FUser;
  end;

implementation

{ TRespalUserContext }

procedure TRespalUserContext.SetCustomData(AValue: TStringList);
begin
if FCustomData<>AValue then
  FCustomData.Assign(AValue);
end;

constructor TRespalUserContext.Create;
begin
FRoles:=TStringList.Create;
FCustomData:=TStringList.Create;
FUserName:='';
FLoggedSince:=0;
FRealm:='';
end;

destructor TRespalUserContext.Destroy;
begin
FRoles.Clear;
FRoles.Free;
FCustomData.Clear;
FCustomData.Free;
inherited Destroy;
end;

{ TRespalContext }

function TRespalContext.GetSession: TRespalSession;
begin
Result:=FSession.GetSession;
end;

constructor TRespalContext.Create(AContext: TIdContext; ARequest: TRespalRequest;
  AResponse: TRespalResponse; ASession: TRespalSessionContext);
begin
FContext:=AContext;
FRequest:=ARequest;
FResponse:=AResponse;
FSession:=ASession;
FUser:=nil;
end;

destructor TRespalContext.Destroy;
begin
inherited Destroy;
end;

function TRespalContext.CreateSession: TRespalSession;
begin
Result:=FSession.CreateSession;
end;

end.

