unit Respal.Session;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdCustomHTTPServer, IdHTTPServer, IdContext;

type

  { TRespalHTTPSessionList }

  TRespalHTTPSessionList = class(TIdHTTPDefaultSessionList)
  public
    function GetSession(const SessionID, RemoteIP: string): TIdHTTPSession; override;
  end;

  TRespalSessionManager = class;

  { TRespalSession }

  TRespalSession = class
  private
    FManager: TRespalSessionManager;
    FIdHTTPSession: TIdHTTPSession;
    procedure ClearObjects;
    function GetItensCount: Integer;
    function GetNames(AIndex: Integer): String;
    function GetObjects(AName: String): TObject;
    function GetValues(AName: String): String;
    procedure SetObjects(AName: String; AValue: TObject);
    procedure SetValues(AName: String; AValue: String);
  public
    constructor Create(AManager: TRespalSessionManager;
      AIdHTTPSession: TIdHTTPSession); reintroduce;
    destructor Destroy; override;
    property Values[AName: String]: String read GetValues write SetValues;
    property Objects[AName: String]: TObject read GetObjects write SetObjects;
    property ItensCount: Integer read GetItensCount;
    property Names[AIndex: Integer]: String read GetNames;
  end;

  { TRespalSessionContext }

  TRespalSessionContext = class
  private
    FManager: TRespalSessionManager;
    FIdContext: TIdContext;
    FIdHTTPRequestInfo: TIdHTTPRequestInfo;
    FIdHTTPResponseInfo: TIdHTTPResponseInfo;
    function GetSessionCreated: Boolean;
  public
    constructor Create(AManager: TRespalSessionManager; AIdContext: TIdContext;
      AIdHTTPRequestInfo: TIdHTTPRequestInfo;
      AIdHTTPResponseInfo: TIdHTTPResponseInfo); reintroduce;
    function CreateSession: TRespalSession;
    function GetSession: TRespalSession;
    property SessionCreated: Boolean read GetSessionCreated;
  end;

  { TRespalSessionManager }

  TRespalSessionManager = class(TComponent)
  private
    FSessions: TList;
    FIdHTTPServer: TIdHTTPServer;
    function FindSession(AIdHTTPSession: TIdHTTPSession): TRespalSession;
    function CreateSession(AIdContext: TIdContext;
      AIdHTTPRequestInfo: TIdHTTPRequestInfo;
      AIdHTTPResponseInfo: TIdHTTPResponseInfo): TRespalSession;
    procedure RemoveSession(ASession: TRespalSession); overload;
  public
    constructor Create(AIdHTTPServer: TIdHTTPServer); reintroduce;
    destructor Destroy; override;
    procedure RemoveSession(AIdHTTPSession: TIdHTTPSession); overload;
  end;

implementation

{ TRespalHTTPSessionList }

function TRespalHTTPSessionList.GetSession(const SessionID, RemoteIP: string
  ): TIdHTTPSession;
begin
Result:=inherited GetSession(SessionID, '');
end;

{ TRespalSessionContext }

function TRespalSessionContext.GetSessionCreated: Boolean;
begin
Result:=Assigned(GetSession);
end;

constructor TRespalSessionContext.Create(AManager: TRespalSessionManager;
  AIdContext: TIdContext; AIdHTTPRequestInfo: TIdHTTPRequestInfo;
  AIdHTTPResponseInfo: TIdHTTPResponseInfo);
begin
FManager:=AManager;
FIdContext:=AIdContext;
FIdHTTPRequestInfo:=AIdHTTPRequestInfo;
FIdHTTPResponseInfo:=AIdHTTPResponseInfo;
end;

function TRespalSessionContext.CreateSession: TRespalSession;
begin
Result:=FManager.CreateSession(FIdContext, FIdHTTPRequestInfo, FIdHTTPResponseInfo);
end;

function TRespalSessionContext.GetSession: TRespalSession;
var
  IdSession: TIdHTTPSession;
begin
IdSession:=FIdHTTPRequestInfo.Session;
if Assigned(IdSession) then
  Result:=FManager.FindSession(IdSession)
else
  Result:=nil;
end;

{ TRespalSessionManager }

function TRespalSessionManager.FindSession(AIdHTTPSession: TIdHTTPSession
  ): TRespalSession;
var
  I: Integer;
begin
Result:=nil;
I:=0;
while (not Assigned(Result)) and (I<FSessions.Count) do
  begin
  if TRespalSession(FSessions[I]).FIdHTTPSession=AIdHTTPSession then
    Result:=TRespalSession(FSessions[I])
  else
    Inc(I);
  end;
end;

constructor TRespalSessionManager.Create(AIdHTTPServer: TIdHTTPServer);
begin
inherited Create(AIdHTTPServer);
FIdHTTPServer:=AIdHTTPServer;
FSessions:=TList.Create;
end;

destructor TRespalSessionManager.Destroy;
begin
FSessions.Clear;
FSessions.Free;
inherited Destroy;
end;

procedure TRespalSessionManager.RemoveSession(AIdHTTPSession: TIdHTTPSession);
var
  Session: TRespalSession;
begin
Session:=FindSession(AIdHTTPSession);
if Assigned(Session) then
  begin
  FSessions.Remove(Session);
  Session.Free;
  end;
end;

function TRespalSessionManager.CreateSession(AIdContext: TIdContext;
  AIdHTTPRequestInfo: TIdHTTPRequestInfo;
  AIdHTTPResponseInfo: TIdHTTPResponseInfo): TRespalSession;
begin
Result:=TRespalSession.Create(
  Self, FIdHTTPServer.CreateSession(AIdContext, AIdHTTPResponseInfo, AIdHTTPRequestInfo)
);
end;

procedure TRespalSessionManager.RemoveSession(ASession: TRespalSession);
begin
if FSessions.IndexOf(ASession)>-1 then
  FSessions.Remove(ASession);
end;

{ TRespalSession }

procedure TRespalSession.ClearObjects;
var
  Obj: TObject;
  I: Integer;
begin
I:=0;
while I<GetItensCount do
  begin
  Obj:=FIdHTTPSession.Content.Objects[I];
  if Assigned(Obj) then
    begin
    FIdHTTPSession.Content.Delete(I);
    Obj.Free;
    end
  else
    Inc(I);
  end;
end;

function TRespalSession.GetItensCount: Integer;
begin
Result:=FIdHTTPSession.Content.Count;
end;

function TRespalSession.GetNames(AIndex: Integer): String;
begin
Result:=FIdHTTPSession.Content.Names[AIndex];
end;

function TRespalSession.GetObjects(AName: String): TObject;
begin
Result:=FIdHTTPSession.Content.Objects[FIdHTTPSession.Content.IndexOfName(AName)];
end;

function TRespalSession.GetValues(AName: String): String;
begin
Result:=FIdHTTPSession.Content.Values[AName];
end;

procedure TRespalSession.SetObjects(AName: String; AValue: TObject);
begin
FIdHTTPSession.Content.AddObject(AName, AValue);
end;

procedure TRespalSession.SetValues(AName: String; AValue: String);
begin
FIdHTTPSession.Content.Values[AName]:=AValue;
end;

constructor TRespalSession.Create(AManager: TRespalSessionManager;
  AIdHTTPSession: TIdHTTPSession);
begin
FManager:=AManager;
FIdHTTPSession:=AIdHTTPSession;
end;

destructor TRespalSession.Destroy;
begin
ClearObjects;
FManager.RemoveSession(Self);
inherited Destroy;
end;

end.

