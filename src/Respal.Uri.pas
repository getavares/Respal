unit Respal.Uri;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Respal.Regex.Utils, contnrs, RegExpr;

type
  TRespalUriParameterType = (ratAny, ratString, ratBoolean, ratInteger, ratDate,
    ratTime, ratDateTime);

  { TRespalUriParameter }

  TRespalUriParameter = class
  private
    FName: String;
    FUriPart: String;
  public
    constructor Create(AName: String; AUriPart: String); virtual;
    property Name: String read FName;
    property UriPart: String read FUriPart;
  end;

  { TRespalUriRouteParameter }

  TRespalUriRouteParameter = class(TRespalUriParameter)
  private
    FIndexPart: Integer;
    FType: TRespalUriParameterType;
    FValue: Variant;
  public
    constructor Create(AName: String; AUriPart: String; AIndexPart: Integer; AType: TRespalUriParameterType); reintroduce;
    property IndexPart: Integer read FIndexPart;
    property ParamType: TRespalUriParameterType read FType;
    property Value: Variant read FValue;
  end;

  { TRespalUriValueParameter }

  TRespalUriValueParameter = class(TRespalUriParameter)
  private
    FValue: String;
  public
    constructor Create(AName: String; AValue: String); reintroduce;
    property Value: String read FValue;
  end;

  { TRespalUriValueParameterList }

  TRespalUriValueParameterList = class
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItem(AName: String): TRespalUriValueParameter;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TRespalUriValueParameter): Integer;
    function Remove(AItem: TRespalUriValueParameter): Integer;
    procedure Clear;
    property Items[AName: String]: TRespalUriValueParameter read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TRespalUri = class
  private
    FRouteParams: TObjectList;
    FUri: String;
    FValueParams: TRespalUriValueParameterList;
    FUriParts: array of String;
    procedure FillRouteParams;
    procedure FillValueParams;
    function GetUriParts(AIndex: Integer): String;
    function GetUriPartsCount: Integer;
    function StringToParamType(S: String): TRespalUriParameterType;
    function RouteParamByPartIndex(AIndex: Integer): TRespalUriRouteParameter;
  public
    constructor Create(AUri: String);
    destructor Destroy; override;
    function CompatibleUri(AUri: TRespalUri): Boolean; overload;
    function CompatibleUri(AUri: String): Boolean; overload;
    function ParseRouteParamsSchema(AUri: TRespalUri; AChangeCurrentParams: Boolean = True): Boolean;
    property ToString: String read FUri;
    property UriParts[AIndex: Integer]: String read GetUriParts;
    property UriPartsCount: Integer read GetUriPartsCount;
    property RouteParams: TObjectList read FRouteParams;
    property ValueParams: TRespalUriValueParameterList read FValueParams;
  end;

const
  URI_ROUTER_WILDCARD = '*';
  URI_ROUTER_VARIABLE = '$';
  URI_ROUTER_STRING_ARGUMENT = 'str';
  URI_ROUTER_BOOLEAN_ARGUMENT = 'bool';
  URI_ROUTER_INTEGER_ARGUMENT = 'int';
  URI_ROUTER_DATE_ARGUMENT = 'date';
  URI_ROUTER_TIME_ARGUMENT = 'time';
  URI_ROUTER_DATETIME_ARGUMENT = 'datetime';

implementation

{ TRespalUriValueParameterList }

function TRespalUriValueParameterList.GetItem(AName: String
  ): TRespalUriValueParameter;
var
  I: Integer;
begin
I:=0;
Result:=nil;
while (not Assigned(Result)) and (I<Count) do
  begin
  if TRespalUriValueParameter(FItems[I]).Name=AName then
    Result:=TRespalUriValueParameter(FItems[I])
  else
    Inc(I);
  end;
end;

function TRespalUriValueParameterList.GetCount: Integer;
begin
Result:=FItems.Count;
end;

constructor TRespalUriValueParameterList.Create;
begin
FItems:=TObjectList.Create;
end;

destructor TRespalUriValueParameterList.Destroy;
begin
FItems.Clear;
FItems.Free;
inherited Destroy;
end;

function TRespalUriValueParameterList.Add(AItem: TRespalUriValueParameter): Integer;
begin
Result:=FItems.Add(AItem);
end;

function TRespalUriValueParameterList.Remove(AItem: TRespalUriValueParameter): Integer;
begin
Result:=FItems.Remove(AItem);
end;

procedure TRespalUriValueParameterList.Clear;
begin
FItems.Clear;
end;

{ TRespalUri }

procedure TRespalUri.FillRouteParams;
var
  RegexWords: TRegExpr;
  RegexNamesWords: TRegExpr;
  RouteParam: TRespalUriRouteParameter;
  ParamName: String;
  LUri: String;
  ParamType: TRespalUriParameterType;
  UriSplit: TStringList;
  I: Integer;
  PosParams: Integer;
begin
RegexWords:=TRegExpr.Create(TRespalRegexUtils.ExprWordsInText([
  '<'+URI_ROUTER_VARIABLE+'>',
  '<'+URI_ROUTER_STRING_ARGUMENT+'>',
  '<'+URI_ROUTER_BOOLEAN_ARGUMENT+'>',
  '<'+URI_ROUTER_INTEGER_ARGUMENT+'>',
  '<'+URI_ROUTER_DATE_ARGUMENT+'>',
  '<'+URI_ROUTER_TIME_ARGUMENT+'>',
  '<'+URI_ROUTER_DATETIME_ARGUMENT+'>'], 1, True, True));
RegexNamesWords:=TRegExpr.Create(TRespalRegexUtils.ExprNamedRoutesParams([
  URI_ROUTER_VARIABLE,
  URI_ROUTER_STRING_ARGUMENT,
  URI_ROUTER_BOOLEAN_ARGUMENT,
  URI_ROUTER_INTEGER_ARGUMENT,
  URI_ROUTER_DATE_ARGUMENT,
  URI_ROUTER_TIME_ARGUMENT,
  URI_ROUTER_DATETIME_ARGUMENT]));
UriSplit:=TStringList.Create;
try
  UriSplit.StrictDelimiter:=True;
  UriSplit.Delimiter:='/';
  LUri:=FUri;
  PosParams:=Pos('?', LUri);
  if PosParams>0 then
    LUri:=Copy(LUri, 1, PosParams-1);
  UriSplit.DelimitedText:=LUri;
  for I:=0 to UriSplit.Count do
    begin
    SetLength(FUriParts, Length(FUriParts)+1);
    FUriParts[High(FUriParts)]:=UriSplit[I];
    RegexWords.Exec(UriSplit[I]);
    if RegexWords.SubExprMatchCount=1 then
      begin
      ParamType:=StringToParamType(UriSplit[I]);
      RouteParam:=TRespalUriRouteParameter.Create('', UriSplit[I], I, ParamType);
      FRouteParams.Add(RouteParam);
      end
    else
      begin
      RegexNamesWords.Exec(UriSplit[I]);
      if RegexNamesWords.SubExprMatchCount=1 then
        begin
        ParamName:=Copy(UriSplit[I], RegexNamesWords.MatchPos[0], RegexNamesWords.MatchLen[0]);
        ParamType:=StringToParamType(UriSplit[I]);
        RouteParam:=TRespalUriRouteParameter.Create(ParamName, UriSplit[I], I, ParamType);
        end;
      end;
    end;
finally
  UriSplit.Free;
  RegexWords.Free;
  end;
end;

procedure TRespalUri.FillValueParams;
var
  ValueParam: TRespalUriValueParameter;
  LUri: String;
  UriSplit: TStringList;
  I: Integer;
  PosParams: Integer;
begin
LUri:=FUri;
PosParams:=Pos('?', LUri);
if PosParams>0 then
  begin
  LUri:=Copy(LUri, PosParams+1, Length(LUri)-PosParams-1);
  UriSplit:=TStringList.Create;
  try
    UriSplit.StrictDelimiter:=True;
    UriSplit.Delimiter:='&';
    UriSplit.DelimitedText:=LUri;
    for I:=0 to UriSplit.Count do
      begin
      ValueParam:=TRespalUriValueParameter.Create(UriSplit.Names[I], UriSplit.ValueFromIndex[I]);
      FValueParams.Add(ValueParam);
      end;
  finally
    UriSplit.Free;
    end;
  end;
end;

function TRespalUri.GetUriParts(AIndex: Integer): String;
begin
Result:=FUriParts[AIndex];
end;

function TRespalUri.GetUriPartsCount: Integer;
begin
Result:=Length(FUriParts);
end;

function TRespalUri.StringToParamType(S: String): TRespalUriParameterType;
begin
if SameText(S, '<'+URI_ROUTER_VARIABLE+'>') or (Pos(':'+URI_ROUTER_VARIABLE+'>', S)>0) then
  Result:=ratAny
else if SameText(S, '<'+URI_ROUTER_STRING_ARGUMENT+'>') or (Pos(':'+URI_ROUTER_STRING_ARGUMENT+'>', S)>0) then
  Result:=ratString
else if SameText(S, '<'+URI_ROUTER_BOOLEAN_ARGUMENT+'>') or (Pos(':'+URI_ROUTER_BOOLEAN_ARGUMENT+'>', S)>0) then
  Result:=ratBoolean
else if SameText(S, '<'+URI_ROUTER_INTEGER_ARGUMENT+'>') or (Pos(':'+URI_ROUTER_INTEGER_ARGUMENT+'>', S)>0) then
  Result:=ratInteger
else if SameText(S, '<'+URI_ROUTER_DATE_ARGUMENT+'>') or (Pos(':'+URI_ROUTER_DATE_ARGUMENT+'>', S)>0) then
  Result:=ratDate
else if SameText(S, '<'+URI_ROUTER_TIME_ARGUMENT+'>') or (Pos(':'+URI_ROUTER_TIME_ARGUMENT+'>', S)>0) then
  Result:=ratTime
else if SameText(S, '<'+URI_ROUTER_DATETIME_ARGUMENT+'>') or (Pos(':'+URI_ROUTER_DATETIME_ARGUMENT+'>', S)>0) then
  Result:=ratDateTime;
end;

function TRespalUri.RouteParamByPartIndex(AIndex: Integer): TRespalUriRouteParameter;
var
  I: Integer;
begin
Result:=nil;
I:=0;
while (not Assigned(Result)) and (I<FRouteParams.Count) do
  begin
  if TRespalUriRouteParameter(FRouteParams[I]).IndexPart=AIndex then
    Result:=TRespalUriRouteParameter(FRouteParams[I])
  else
    Inc(I);
  end;
end;

constructor TRespalUri.Create(AUri: String);
begin
FUri:=AUri;
FRouteParams:=TObjectList.Create;
FValueParams:=TRespalUriValueParameterList.Create;
end;

destructor TRespalUri.Destroy;
begin
FRouteParams.Clear;
FRouteParams.Free;
FValueParams.Clear;
FValueParams.Free;
inherited Destroy;
end;

function TRespalUri.CompatibleUri(AUri: TRespalUri): Boolean;
begin
Result:=ParseRouteParamsSchema(AUri, False);
end;

function TRespalUri.CompatibleUri(AUri: String): Boolean;
var
  LUri: TRespalUri;
begin
LUri:=TRespalUri.Create(AUri);
try
  Result:=Self.CompatibleUri(LUri);
finally
  LUri.Free;
  end;
end;

function TRespalUri.ParseRouteParamsSchema(AUri: TRespalUri;
  AChangeCurrentParams: Boolean): Boolean;
var
  I: Integer;
  Param: TRespalUriRouteParameter;
  ParamType: TRespalUriParameterType;
  NewParam: TRespalUriRouteParameter;
  Converted: Variant;

begin
Result:=True;
if AChangeCurrentParams then
  FRouteParams.Clear;
if FUri<>'*' then
  begin
  I:=Low(FUriParts);
  Result:=AUri.UriPartsCount=UriPartsCount;
  while (Result) and (I<=High(FUriParts)) do
    begin
    Param:=AUri.RouteParamByPartIndex(I);
    if Assigned(Param) then
      begin
      try
        if Param.ParamType=ratBoolean then
          begin
          Converted:=StrToBool(AUri.UriParts[I]);
          ParamType:=ratBoolean;
          end
        else if Param.ParamType=ratTime then
          begin
          Converted:=StrToTime(AUri.UriParts[I]);
          ParamType:=ratTime;
          end
        else if Param.ParamType=ratDate then
          begin
          Converted:=StrToDate(AUri.UriParts[I]);
          ParamType:=ratDate;
          end
        else if Param.ParamType=ratDateTime then
          begin
          Converted:=StrToDateTime(AUri.UriParts[I]);
          ParamType:=ratDateTime;
          end
        else if Param.ParamType=ratInteger then
          begin
          try
            Converted:=StrToInt(AUri.UriParts[I]);
          except
            Converted:=StrToInt64(AUri.UriParts[I]);
            end;
          ParamType:=ratInteger;
          end;
        if AChangeCurrentParams then
          begin
          NewParam:=TRespalUriRouteParameter.Create(Param.Name, FUriParts[I], I, ParamType);
          NewParam.FValue:=Converted;
          FRouteParams.Add(NewParam);
          end;
        Result:=True;
      except
        Result:=False;
        end;
      end
    else
      Result:=SameText(FUriParts[I], AUri.UriParts[I]);
    Inc(I);
    end;
  end;
end;

{ TRespalUriValueParameter }

constructor TRespalUriValueParameter.Create(AName: String; AValue: String);
begin
inherited Create(AName, '');
FValue:=AValue;
end;

{ TRespalUriRouteParameter }

constructor TRespalUriRouteParameter.Create(AName: String; AUriPart: String;
  AIndexPart: Integer; AType: TRespalUriParameterType);
begin
inherited Create(AName, AUriPart);
FValue:=Unassigned;
FIndexPart:=AIndexPart;
FType:=AType;
end;

{ TRespalUriParameter }

constructor TRespalUriParameter.Create(AName: String; AUriPart: String);
begin
FName:=AName;
FUriPart:=AUriPart;
end;

end.

