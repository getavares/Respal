unit ucache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils;

type
  TCacheControl = class;

  { TCacheObject }

  TCacheObject = class(TPersistent)
  private
    FCacheControl: TCacheControl;
    FCreatedTime: TDateTime;
    FIdentity: String;
    FInvalid: Boolean;
    FDuration: Integer;
    function GetExpiration: TDateTime;
    function GetIsValid: Boolean;
    procedure DoRenew;
  protected
    procedure Renew(var ACreateTime: TDateTime; var ADuration: Integer); virtual;
  public
    constructor Create(ACacheControl: TCacheControl; AIdentity: String); virtual;
    procedure Invalidate;
    property Identity: String read FIdentity;
    property CreatedTime: TDateTime read FCreatedTime;
    property Expiration: TDateTime read GetExpiration;
    property Duration: Integer read FDuration;
    property IsValid: Boolean read GetIsValid;
  end;

  TCacheObjectSetup = procedure(const ACacheObject: TCacheObject);

  TCacheObjectClass = class of TCacheObject;

  { TThreadCacheControl }

  TThreadCacheControl = class(TThread)
  private
    FControl: TCacheControl;
    FEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AControl: TCacheControl); reintroduce;
    destructor Destroy; override;
    procedure Finalize;
  end;

  { TCacheControl }

  TCacheControl = class
  private
    FSection: TCriticalSection;
    FThread: TThreadCacheControl;
    FLastRemove: TDateTime;
    FList: TStringList;
    procedure RemoveExpiredObject;
  protected
    FCacheObjectClass: TCacheObjectClass;
    function GetItem(AIdentity: String): TCacheObject; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(AIdentity: String; ASetup: TCacheObjectSetup = nil): TCacheObject;
    function HasItem(AIdentity: String): Boolean;
    procedure Remove(AIdentity: String); overload;
    procedure Remove(AObject: TCacheObject); overload;
    procedure Verify;
  end;

implementation

{ TCacheObject }

function TCacheObject.GetIsValid: Boolean;
begin
Result:=(not FInvalid) or (Now>IncMinute(FCreatedTime, FDuration));
end;

procedure TCacheObject.DoRenew;
var
  LCreatedTime: TDateTime;
  LDuration: Integer;
  Changed: Boolean;
begin
Changed:=False;
LCreatedTime:=FCreatedTime;
LDuration:=FDuration;
Renew(LCreatedTime, LDuration);
if LCreatedTime>FCreatedTime then
  begin
  FCreatedTime:=LCreatedTime;
  Changed:=True;
  end;
if LDuration>FDuration then
  begin
  FDuration:=LDuration;
  Changed:=True;
  end;
if Changed then
  FCacheControl.Verify;
end;

procedure TCacheObject.Renew(var ACreateTime: TDateTime; var ADuration: Integer
  );
begin
end;

function TCacheObject.GetExpiration: TDateTime;
begin
Result:=IncMinute(FCreatedTime, FDuration);
end;

constructor TCacheObject.Create(ACacheControl: TCacheControl; AIdentity: String
  );
begin
FCacheControl:=ACacheControl;
FIdentity:=AIdentity;
FInvalid:=False;
FCreatedTime:=Now;
FDuration:=15;
end;

procedure TCacheObject.Invalidate;
begin
FInvalid:=True;
FCacheControl.Verify;
end;

{ TCacheControl }

procedure TCacheControl.RemoveExpiredObject;
var
  Item: TCacheObject;
  RemovedItem: Boolean;
  I: Integer;
begin
if MinuteSpan(FLastRemove, Now)>1 then
  begin
  FSection.Enter;
  try
    FLastRemove:=Now;
    I:=0;
    while (I<FList.Count) do
      begin
      RemovedItem:=False;
      Item:=TCacheObject(FList.Objects[I]);
      if Assigned(Item) then
        begin
        if (not Item.IsValid) then
          begin
          FList.Delete(I);
          Item.Free;
          RemovedItem:=True;
          end
        end;
      if (not RemovedItem) then
        Inc(I);
      end;
  finally
    FSection.Leave;
    end;
  end;
end;

function TCacheControl.HasItem(AIdentity: String): Boolean;
begin
FSection.Enter;
try
  Result:=FList.IndexOf(AIdentity)>-1;
finally
  FSection.Leave;
  end;
end;

function TCacheControl.GetItem(AIdentity: String): TCacheObject;
var
  Index: Integer;
begin
Result:=nil;
FSection.Enter;
try
  Index:=FList.IndexOf(AIdentity);
  if (Index>-1) then
    begin
    if TCacheObject(FList.Objects[Index]).IsValid then
      begin
      Result:=TCacheObject(FList.Objects[Index]);
      Result.DoRenew;
      end;
    end;
finally
  FSection.Leave;
  end;
end;

constructor TCacheControl.Create;
begin
FCacheObjectClass:=TCacheObject;
FSection:=TCriticalSection.Create;
FThread:=TThreadCacheControl.Create(Self);
end;

destructor TCacheControl.Destroy;
begin
FThread.Finalize;
FSection.Free;
inherited Destroy;
end;

function TCacheControl.Add(AIdentity: String; ASetup: TCacheObjectSetup
  ): TCacheObject;
begin
Result:=nil;
FSection.Enter;
try
  if FList.IndexOf(AIdentity)=-1 then
    begin
    Result:=FCacheObjectClass.Create(Self, AIdentity);
    if Assigned(ASetup) then
      ASetup(Result);
    FList.AddObject(Result.Identity, Result);
    end
  else
    raise Exception.Create('CacheObject already exists');
finally
  FSection.Leave;
  end;
end;

procedure TCacheControl.Remove(AIdentity: String);
var
  Item: TCacheObject;
  Index: Integer;
begin
FSection.Enter;
try
  Index:=FList.IndexOf(AIdentity);
  if Index>-1 then
    begin
    Item:=TCacheObject(FList.Objects[Index]);
    FList.Delete(Index);
    Item.Free;
    end;
finally
  FSection.Leave;
  end;
end;

procedure TCacheControl.Remove(AObject: TCacheObject);
begin
Remove(AObject.Identity);
end;

procedure TCacheControl.Verify;
begin
FThread.FEvent.SetEvent;
end;

{ TThreadCacheControl }

procedure TThreadCacheControl.Execute;
begin
while (not Terminated) do
  begin
  FControl.RemoveExpiredObject;
  FEvent.WaitFor(60000);
  FEvent.ResetEvent;
  end;
end;

constructor TThreadCacheControl.Create(AControl: TCacheControl);
begin
inherited Create(True);
FreeOnTerminate:=True;
FEvent:=TEvent.Create(nil, False, False, '');
FControl:=AControl;
end;

destructor TThreadCacheControl.Destroy;
begin
FEvent.Free;
inherited Destroy;
end;

procedure TThreadCacheControl.Finalize;
begin
Terminate;
FEvent.SetEvent;
end;

end.

