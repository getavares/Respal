unit Respal.Router;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdCustomHTTPServer, Respal.Route, Respal.Controller,
  Contnrs;

type
  { TRespalRouter }

  TRespalRouter = class
  protected
    FRoutes: TObjectList;
    procedure Configure; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function Instance: TRespalRouter; static;
    function FindRoute(AHttpVerb: THTTPCommandType; AUri: String): TRespalRoute;
    procedure AddRoute(AHttpVerb: THTTPCommandType; AUri: String;
      AControllerClass: TRespalControllerClass; AMethod: String);
    procedure AddRoute(ARoute: TRespalRoute);
    procedure RemoveRoute(AHttpVerb: THTTPCommandType; AUri: String); overload;
    procedure Removeroute(ARoute: TRespalRoute); overload;
  end;

  TRespalRouterClass = class of TRespalRouter;

var
  RespalRouterClass: TRespalRouterClass;

implementation

var
  RespalRouter: TRespalRouter;

{ TRespalRouter }

procedure TRespalRouter.Configure;
begin
end;

constructor TRespalRouter.Create;
begin
FRoutes:=TObjectList.Create;
end;

destructor TRespalRouter.Destroy;
begin
FRoutes.Clear;
FRoutes.Free;
inherited Destroy;
end;

class function TRespalRouter.Instance: TRespalRouter;
begin
if (not Assigned(RespalRouter)) then
  RespalRouter:=TRespalRouter.Create;
Result:=RespalRouter;
end;

function TRespalRouter.FindRoute(AHttpVerb: THTTPCommandType;
  AUri: String): TRespalRoute;
var
  I: Integer;
begin
I:=0;
Result:=nil;
while (not Assigned(Result)) and (I<FRoutes.Count) do
  begin
  if (TRespalRoute(FRoutes[I]).HttpVerb=AHttpVerb) and TRespalRoute(FRoutes[I]).Uri.CompatibleUri(AUri) then
    Result:=TRespalRoute(FRoutes[I]);
  Inc(I);
  end;
end;

procedure TRespalRouter.AddRoute(AHttpVerb: THTTPCommandType; AUri: String;
  AControllerClass: TRespalControllerClass; AMethod: String);
var
  Route: TRespalRoute;
begin
Route:=FindRoute(AHttpVerb, AUri);
if (not Assigned(Route)) then
  begin
  Route:=TRespalRoute.Create(AHttpVerb, AUri, AControllerClass, AMethod);
  FRoutes.Add(Route);
  end;
end;

procedure TRespalRouter.AddRoute(ARoute: TRespalRoute);
var
  Route: TRespalRoute;
begin
Route:=FindRoute(ARoute.HttpVerb, ARoute.Uri.ToString);
if (not Assigned(Route)) then
  FRoutes.Add(ARoute);
end;

procedure TRespalRouter.RemoveRoute(AHttpVerb: THTTPCommandType; AUri: String);
var
  Route: TRespalRoute;
begin
Route:=FindRoute(AHttpVerb, AUri);
if Assigned(Route) then
  RemoveRoute(Route);
end;

procedure TRespalRouter.Removeroute(ARoute: TRespalRoute);
begin
FRoutes.Remove(ARoute);
end;

initialization

RespalRouter:=nil;

finalization

if Assigned(RespalRouter) then
  RespalRouter.Free;

end.

