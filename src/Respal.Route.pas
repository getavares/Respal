unit Respal.Route;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdCustomHTTPServer, Respal.Uri, Respal.Controller;

type
  TRespalRoute = class
  private
    FHttpVerb: THTTPCommandType;
    FControllerClass: TRespalControllerClass;
    FMethod: String;
    FUri: TRespalUri;
  public
    constructor Create(AHttpVerb: THTTPCommandType; AUri: String;
      AControllerClass: TRespalControllerClass; AMethod: String);
    destructor Destroy; override;
    property HttpVerb: THTTPCommandType read FHttpVerb;
    property ControllerClass: TRespalControllerClass read FControllerClass;
    property Method: String read FMethod;
    property Uri: TRespalUri read FUri;
  end;

implementation

{ TRespalRoute }

constructor TRespalRoute.Create(AHttpVerb: THTTPCommandType; AUri: String;
  AControllerClass: TRespalControllerClass; AMethod: String);
begin
FUri:=TRespalUri.Create(AUri);
FHttpVerb:=AHttpVerb;
FControllerClass:=AControllerClass;
FMethod:=AMethod;
end;

destructor TRespalRoute.Destroy;
begin
FUri.Free;
inherited Destroy;
end;

end.

