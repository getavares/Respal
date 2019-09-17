unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Respal.Core, IdHTTPServer;

type

  { TForm1 }

  TForm1 = class(TForm)
    IdHTTPServer1: TIdHTTPServer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCore: TRespalCore;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCore := TRespalCore.Create(Self);
  FCore.Initialize;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCore.Finalize;
end;

end.

