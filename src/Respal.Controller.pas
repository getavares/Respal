unit Respal.Controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Respal.Context;

type

  TRespalControllerAction = procedure of object;

  { TRespalController }

  TRespalController = class
  private
    FContext: TRespalContext;
  protected
    property Context: TRespalContext read FContext;
  public
    constructor Create(AContext: TRespalContext); virtual;
    destructor Destroy; override;
  end;

  TRespalControllerClass = class of TRespalController;

implementation

{ TRespalController }

constructor TRespalController.Create(AContext: TRespalContext);
begin
FContext:=AContext;
end;

destructor TRespalController.Destroy;
begin
inherited Destroy;
end;

end.

