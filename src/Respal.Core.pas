unit Respal.Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IdHTTPServer, IdCustomHTTPServer, IdContext,
  Respal.Router, Respal.Context, Respal.Controller, Respal.Route,
  Respal.Request, Respal.Response, Respal.Session;

type

  { TRespalCore }

  TRespalCore = class(TComponent)
  private
    FServer: TIdHTTPServer;
    FSessionManager: TRespalSessionManager;
    procedure ServerCommandGet(AContext: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure ServerSessionEnd(Sender: TIdHTTPSession);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Initialize;
    procedure Finalize;
  end;

implementation

{ TRespalCore }

procedure TRespalCore.ServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Route: TRespalRoute;
  Context: TRespalContext;
  Request: TRespalRequest;
  Response: TRespalResponse;
  Controller: TRespalController;
  ControllerMethod: TMethod;
  SessionContext: TRespalSessionContext;
begin
  Request:=TRespalRequest.Create(ARequestInfo);
  Response:=TRespalResponse.Create(AResponseInfo);
  try
    try
      Route:=TRespalRouter.Instance.FindRoute(ARequestInfo.CommandType, ARequestInfo.URI);
      if Assigned(Route) then
        begin
          SessionContext:=TRespalSessionContext.Create(FSessionManager, AContext, ARequestInfo, AResponseInfo);
          try
            Request.Uri.ParseRouteParamsSchema(Route.Uri);
            Context:=TRespalContext.Create(AContext, Request, Response, SessionContext);
            try
              Controller:=Route.ControllerClass.Create(Context);
              ControllerMethod.Code:=Controller.MethodAddress(Route.Method);
              ControllerMethod.Data:=Controller;
              TRespalControllerAction(ControllerMethod);
            finally
              Context.Free;
            end;
          finally
            SessionContext.Free;
          end;
        end
      else
        Response.ClientError.NotFound;
    except
      on E: Exception do
      begin
        Response.ServerError.InternalServerError;
      end;
    end;
  finally
    Request.Free;
    Response.Free;
  end;
end;

procedure TRespalCore.ServerSessionEnd(Sender: TIdHTTPSession);
begin
  FSessionManager.RemoveSession(Sender);
end;

constructor TRespalCore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServer:=TIdHTTPServer.Create(Self);
  FServer.OnCommandGet:=@ServerCommandGet;
  FSessionManager:=TRespalSessionManager.Create(FServer);
end;

procedure TRespalCore.Initialize;
begin
  FServer.Active:=True;
end;

procedure TRespalCore.Finalize;
begin
  FServer.Active:=False;
end;

end.

