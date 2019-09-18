unit Respal.Listeners;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Respal.Route, Respal.Context, IdCustomHTTPServer;

type
  IRespalListener = interface
  end;

  IRespalListenerBeforeFindRoute = interface(IRespalListener)
  ['{4221837B-4ACF-4917-B48C-ED1CCF79F330}']
    procedure OnBeforeFindRoute(var AHttpVerb: THTTPCommandType;
      var AUri: String; var Handled: Boolean; var Proceed: Boolean);
  end;

  IRespalListenerAfterFindRoute = interface(IRespalListener)
  ['{5B92F5BD-5B48-49F5-8727-1CC9CE3A9B57}']
    procedure OnAfterFindRoute(const ARoute: TRespalRoute; var Handled: Boolean);
  end;

  IRespalListenerBeforeGetMethod = interface(IRespalListener)
  ['{FE6AB9AA-8BFE-48BD-886B-320A591F1830}']
    procedure OnBeforeGetAction(const AContext: TRespalContext;
      const ARoute: TRespalRoute; var AMethod: String; var Handled: Boolean;
      var Proceed: Boolean);
  end;

  IRespalListenerBeforeExecuteMethod = interface(IRespalListener)
  ['{69044724-EF41-40AD-9874-F6BDBB76BA5E}']
    procedure OnBeforeExecuteMethod(const AContext: TRespalContext;
      const ARoute: TRespalRoute; const AMethod: String; var Handled: Boolean;
      var Proceed: Boolean);
  end;

  IRespalListenerAfterExecuteMethod = interface(IRespalListener)
  ['{E413EDE2-615B-4195-9416-55240B1F2450}']
    procedure OnAfterExecuteMethod(const AContext: TRespalContext;
      const ARoute: TRespalRoute; const AMethod: String; var Handled: Boolean);
  end;

  TRespalListenerEvent = procedure(AListener: IRespalListener) of object;

  { TRespalListener }

  TRespalListener = class
  private
    FOnAddListener: TRespalListenerEvent;
    FOnRemoveListener: TRespalListenerEvent;
  public
    constructor Create(AOnAddListener: TRespalListenerEvent;
      AOnRemoveListener: TRespalListenerEvent);
    destructor Destroy; override;
    procedure Add(AListener: IRespalListener);
    procedure Remove(AListener: IRespalListener);
  end;

implementation

{ TRespalListener }

constructor TRespalListener.Create(AOnAddListener: TRespalListenerEvent;
  AOnRemoveListener: TRespalListenerEvent);
begin
  FOnAddListener:=AOnAddListener;
  FOnRemoveListener:=AOnRemoveListener;
end;

destructor TRespalListener.Destroy;
begin
  inherited Destroy;
end;

procedure TRespalListener.Add(AListener: IRespalListener);
begin
  if Assigned(FOnAddListener) then
    FOnAddListener(AListener);
end;

procedure TRespalListener.Remove(AListener: IRespalListener);
begin
  if Assigned(FOnRemoveListener) then
    FOnRemoveListener(AListener);
end;

end.
