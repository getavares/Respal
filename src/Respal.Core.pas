unit Respal.Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IdHTTPServer, IdCustomHTTPServer, IdContext,
  Respal.Router, Respal.Context, Respal.Controller, Respal.Route,
  Respal.Request, Respal.Response, Respal.Session, Respal.Listeners;

type
  TRespalCoreConfigureListenersEvent = procedure(AListener: TRespalListener) of object;

  { TRespalCore }

  TRespalCore = class(TComponent)
  private
    FListeners: TRespalListener;
    FBeforeFindRoute: TList;
    FAfterFindRoute: TList;
    FBeforeGetMethod: TList;
    FBeforeExecuteMethod: TList;
    FAfterExecuteMethod: TList;
    FServer: TIdHTTPServer;
    FSessionManager: TRespalSessionManager;
    FOnConfigureListeners: TRespalCoreConfigureListenersEvent;
    procedure ServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure ServerSessionEnd(Sender: TIdHTTPSession);
    function DoBeforeFindRoute(var AHttpVerb: THTTPCommandType;
      var AUri: String): Boolean;
    procedure DoAfterFindRoute(const ARoute: TRespalRoute);
    function DoBeforeGetMethod(const AContext: TRespalContext;
      const ARoute: TRespalRoute; var AMethod: String): Boolean;
    function DoBeforeExecuteMethod(const AContext: TRespalContext;
      const ARoute: TRespalRoute; const AMethod: String): Boolean;
    procedure DoAfterExecuteMethod(const AContext: TRespalContext;
      const ARoute: TRespalRoute; const AMethod: String);
    procedure OnAddListener(AListener: IRespalListener);
    procedure OnRemoveListener(AListener: IRespalListener);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Initialize;
    procedure Finalize;
    property OnConfigureListeners: TRespalCoreConfigureListenersEvent
      read FOnConfigureListeners write FOnConfigureListeners;
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
  HttpVerb: THTTPCommandType;
  URI: String;
  RouteMethod: String;
begin
  Request:=TRespalRequest.Create(ARequestInfo);
  Response:=TRespalResponse.Create(AResponseInfo);
  try
    try
      HttpVerb:=ARequestInfo.CommandType;
      URI:=ARequestInfo.URI;
      if DoBeforeFindRoute(HttpVerb, URI) then
        begin
          Route:=TRespalRouter.Instance.FindRoute(HttpVerb, URI);
          if Assigned(Route) then
            begin
              DoAfterFindRoute(Route);
              SessionContext:=TRespalSessionContext.Create(FSessionManager, AContext, ARequestInfo, AResponseInfo);
              try
                Request.Uri.ParseRouteParamsSchema(Route.Uri);
                Context:=TRespalContext.Create(AContext, Request, Response, SessionContext);
                try
                  RouteMethod:=Route.Method;
                  if DoBeforeGetMethod(Context, Route, RouteMethod) then
                    begin
                      Controller:=Route.ControllerClass.Create(Context);
                      ControllerMethod.Code:=Controller.MethodAddress(RouteMethod);
                      if ControllerMethod.Code=nil then
                        raise Exception.Create('Configured route has no method. Maybe method is not declared as published');
                      ControllerMethod.Data:=Controller;
                      if DoBeforeExecuteMethod(Context, Route, RouteMethod) then
                        begin
                          TRespalControllerAction(ControllerMethod);
                          DoAfterExecuteMethod(Context, Route, RouteMethod);
                        end;
                    end;
                finally
                  Context.Free;
                end;
              finally
                SessionContext.Free;
              end;
            end
          else
            Response.ClientError.NotFound;
        end;
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

function TRespalCore.DoBeforeFindRoute(var AHttpVerb: THTTPCommandType;
  var AUri: String): Boolean;
var
  Handled: Boolean;
  I: Integer;
begin
  Result:=True;
  if FBeforeFindRoute.Count>0 then
    begin
      Handled:=False;
      I:=0;
      while Result and not Handled and (I<FBeforeFindRoute.Count) do
        begin
          IRespalListenerBeforeFindRoute(FBeforeFindRoute[I]).OnBeforeFindRoute(AHttpVerb, AUri, Handled, Result);
        end;
    end;
end;

procedure TRespalCore.DoAfterFindRoute(const ARoute: TRespalRoute);
var
  Handled: Boolean;
  I: Integer;
begin
  if FAfterFindRoute.Count>0 then
    begin
      Handled:=False;
      I:=0;
      while not Handled and (I<FAfterFindRoute.Count) do
        begin
          IRespalListenerAfterFindRoute(FAfterFindRoute[I]).OnAfterFindRoute(ARoute, Handled);
        end;
    end;
end;

function TRespalCore.DoBeforeGetMethod(const AContext: TRespalContext;
  const ARoute: TRespalRoute; var AMethod: String): Boolean;
var
  Handled: Boolean;
  I: Integer;
begin
  Result:=True;
  if FBeforeGetMethod.Count>0 then
    begin
      Handled:=False;
      I:=0;
      while Result and not Handled and (I<FBeforeGetMethod.Count) do
        begin
          IRespalListenerBeforeGetMethod(FBeforeGetMethod[I]).OnBeforeGetAction(AContext, ARoute, AMethod, Handled, Result);
        end;
    end;
end;

function TRespalCore.DoBeforeExecuteMethod(const AContext: TRespalContext;
  const ARoute: TRespalRoute; const AMethod: String): Boolean;
var
  Handled: Boolean;
  I: Integer;
begin
  Result:=True;
  if FBeforeExecuteMethod.Count>0 then
    begin
      Handled:=False;
      I:=0;
      while Result and not Handled and (I<FBeforeExecuteMethod.Count) do
        begin
          IRespalListenerBeforeExecuteMethod(FBeforeExecuteMethod[I]).OnBeforeExecuteMethod(AContext, ARoute, AMethod, Handled, Result);
        end;
    end;
end;

procedure TRespalCore.DoAfterExecuteMethod(const AContext: TRespalContext;
  const ARoute: TRespalRoute; const AMethod: String);
var
  Handled: Boolean;
  I: Integer;
begin
  if FAfterExecuteMethod.Count>0 then
    begin
      Handled:=False;
      I:=0;
      while not Handled and (I<FAfterExecuteMethod.Count) do
        begin
          IRespalListenerAfterExecuteMethod(FBeforeExecuteMethod[I]).OnAfterExecuteMethod(AContext, ARoute, AMethod, Handled);
        end;
    end;
end;

procedure TRespalCore.OnAddListener(AListener: IRespalListener);
begin
  if Supports(AListener, IRespalListenerBeforeFindRoute) then
    FBeforeFindRoute.Add(AListener)
  else if Supports(AListener, IRespalListenerBeforeFindRoute) then
    FAfterFindRoute.Add(AListener)
  else if Supports(AListener, IRespalListenerBeforeGetMethod) then
    FBeforeGetMethod.Add(AListener)
  else if Supports(AListener, IRespalListenerBeforeExecuteMethod) then
    FBeforeExecuteMethod.Add(AListener)
  else if Supports(AListener, IRespalListenerAfterExecuteMethod) then
    FAfterExecuteMethod.Add(AListener)
  else
    raise Exception.Create('Listener not implemented')
end;

procedure TRespalCore.OnRemoveListener(AListener: IRespalListener);
begin
  if Supports(AListener, IRespalListenerBeforeFindRoute) then
    FBeforeFindRoute.Remove(AListener)
  else if Supports(AListener, IRespalListenerBeforeFindRoute) then
    FAfterFindRoute.Remove(AListener)
  else if Supports(AListener, IRespalListenerBeforeGetMethod) then
    FBeforeGetMethod.Remove(AListener)
  else if Supports(AListener, IRespalListenerBeforeExecuteMethod) then
    FBeforeExecuteMethod.Remove(AListener)
  else if Supports(AListener, IRespalListenerAfterExecuteMethod) then
    FAfterExecuteMethod.Remove(AListener)
  else
    raise Exception.Create('Listener not implemented')
end;

constructor TRespalCore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServer:=TIdHTTPServer.Create(Self);
  FServer.OnCommandGet:=@ServerCommandGet;
  FSessionManager:=TRespalSessionManager.Create(FServer);
  FBeforeFindRoute:=TList.Create;
  FAfterFindRoute:=TList.Create;
  FBeforeGetMethod:=TList.Create;
  FBeforeExecuteMethod:=TList.Create;
  FAfterExecuteMethod:=TList.Create;
  FListeners:=TRespalListener.Create(@OnAddListener, @OnRemoveListener);
end;

procedure TRespalCore.Initialize;
begin
  if Assigned(FOnConfigureListeners) then
    FOnConfigureListeners(FListeners);
  FServer.Active:=True;
end;

procedure TRespalCore.Finalize;
begin
  FServer.Active:=False;
end;

end.

