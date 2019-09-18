unit Respal.JWT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TRespalJWT }

  TRespalJWT = class
  private
    FIssuer: Variant;
    FSubject: Variant;
    FAudience: Variant;
    FExpirationTime: Variant;
    FNotBefore: Variant;
    FIssuedAt: Variant;
    FId: Variant;
    function GetAudience: string;
    function GetExpirationTime: TDateTime;
    function GetId: string;
    function GetIssuedAt: TDateTime;
    function GetIssuer: string;
    function GetNotBefore: TDateTime;
    function GetSubject: string;
    procedure SetAudience(AValue: string);
    procedure SetExpirationTime(AValue: TDateTime);
    procedure SetId(AValue: string);
    procedure SetIssuedAt(AValue: TDateTime);
    procedure SetIssuer(AValue: string);
    procedure SetNotBefore(AValue: TDateTime);
    procedure SetSubject(AValue: string);
  public
    constructor Create;
    property Issuer: string read GetIssuer write SetIssuer;
    property Subject: string read GetSubject write SetSubject;
    property Audience: string read GetAudience write SetAudience;
    property ExpirationTime: TDateTime read GetExpirationTime write SetExpirationTime;
    property NotBefore: TDateTime read GetNotBefore write SetNotBefore;
    property IssuedAt: TDateTime read GetIssuedAt write SetIssuedAt;
    property Id: string read GetId write SetId;
  end;

implementation

{ TRespalJWT }

function TRespalJWT.GetAudience: string;
begin
  if FAudience<>Unassigned then
    Result:=FAudience
  else
    Result:='';
end;

function TRespalJWT.GetExpirationTime: TDateTime;
begin
  if FExpirationTime<>Unassigned then
    Result:=FExpirationTime
  else
    Result:=0;
end;

function TRespalJWT.GetId: string;
begin
  if FId<>Unassigned then
    Result:=FId
  else
    Result:='';
end;

function TRespalJWT.GetIssuedAt: TDateTime;
begin
  if FIssuedAt<>Unassigned then
    Result:=FIssuedAt
  else
    Result:=0;
end;

function TRespalJWT.GetIssuer: string;
begin
  if FIssuer<>Unassigned then
    Result:=FIssuer
  else
    Result:='';
end;

function TRespalJWT.GetNotBefore: TDateTime;
begin
  if FNotBefore<>Unassigned then
    Result:=FNotBefore
  else
    Result:=0;
end;

function TRespalJWT.GetSubject: string;
begin
  if FSubject<>Unassigned then
    Result:=FSubject
  else
    Result:='';
end;

procedure TRespalJWT.SetAudience(AValue: string);
begin
  FAudience:=AValue;
end;

procedure TRespalJWT.SetExpirationTime(AValue: TDateTime);
begin
  FExpirationTime:=AValue;
end;

procedure TRespalJWT.SetId(AValue: string);
begin
  FId:=AValue;
end;

procedure TRespalJWT.SetIssuedAt(AValue: TDateTime);
begin
  FIssuedAt:=AValue;
end;

procedure TRespalJWT.SetIssuer(AValue: string);
begin
  FIssuer:=AValue;
end;

procedure TRespalJWT.SetNotBefore(AValue: TDateTime);
begin
  FNotBefore:=AValue;
end;

procedure TRespalJWT.SetSubject(AValue: string);
begin
  FSubject:=AValue;
end;

constructor TRespalJWT.Create;
begin
  FIssuer:=Unassigned;
  FSubject:=Unassigned;
  FAudience:=Unassigned;
  FExpirationTime:=Unassigned;
  FNotBefore:=Unassigned;
  FIssuedAt:=Unassigned;
  FId:=Unassigned;
end;

end.

