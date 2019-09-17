unit ControllerSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Respal.Controller, Respal.Router;

type
  { TControllerSample }
  {$M+}
  TControllerSample = class(TRespalController)
  published
    procedure SampleMethod1;
  end;

implementation

uses
  IdCustomHTTPServer;

{ TControllerLogin }

procedure TControllerSample.SampleMethod1;
begin
  Context.Response.Success.Ok;
end;

initialization

TRespalRouter.Instance.AddRoute(THTTPCommandType.hcPOST, 'sample', TControllerSample, 'SampleMethod1');

end.

