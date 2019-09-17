unit urttiutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo;

type
  TPropertyEnumProc = procedure(Sender: TObject; PropInfo: PPropInfo;
    Storage: TStrings);

  { TRespalRttiUtils }

  TRespalRttiUtils = class
  public
    class procedure EnumProps(Obj: TObject; WithStorage: TStrings;
      Proc: TPropertyEnumProc);
    class function ExtractName(var Buf: Pointer): ShortString;
    class function ExtractParam(var Buf: Pointer): string;
    class procedure Teste(Obj: TObject; Method: String; WithStorage: TStrings);
  end;

procedure WritePropValueToStorage(Sender: TObject; PropInfo: PPropInfo;
  Storage: TStrings);

implementation

class procedure TRespalRttiUtils.EnumProps(Obj: TObject; WithStorage: TStrings;
  Proc: TPropertyEnumProc);
const
  CValidKinds = [tkMethod];
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, i: Integer;

begin
  if (Obj = nil) or (WithStorage = nil) or not Assigned(Proc) then
    Exit;
  PropCount := GetPropList(Obj, PropList);
  if PropCount > 0 then
  try
    for i := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[i];
      if (PropInfo <> nil) and (PropInfo^.PropType^.Kind in CValidKinds) then
        Proc(Obj, PropInfo, WithStorage);
    end;
  finally
    FreeMem(PropList);
  end;
end;

class function TRespalRttiUtils.ExtractName(var Buf: Pointer): ShortString;
begin
  SetLength(Result, Byte(Buf^));
  Inc(Buf, 1);
  if Length(Result) > 0 then
  begin
    Move(Buf^, Result[1], Length(Result));
    Inc(Buf, Length(Result));
  end;
end;

class function TRespalRttiUtils.ExtractParam(var Buf: Pointer): string;
var
  ParamType: record
    Flags: TParamFlags;
    ParamName: ShortString;
    TypeName: ShortString;
  end;
begin
  ParamType.Flags := TParamFlags(Pointer(Buf)^);
  Inc(Buf, SizeOf(TParamFlags));
  ParamType.ParamName := ExtractName(Buf);
  ParamType.TypeName := ExtractName(Buf);
  Result := '';
  if pfVar in ParamType.Flags then
    Result := 'var '
  else
    if pfConst in ParamType.Flags then
      Result := 'const '
    else
      if pfOut in ParamType.Flags then
        Result := 'out ';
  Result := Result + ParamType.ParamName + ': ' + ParamType.TypeName;
end;

class procedure TRespalRttiUtils.Teste(Obj: TObject; Method: String;
  WithStorage: TStrings);
var
  MethodAddr: Pointer;
begin
MethodAddr:=MethodAddress(Method);

end;

procedure WritePropValueToStorage(Sender: TObject; PropInfo: PPropInfo;
  Storage: TStrings);
const
  CMethodKind: array[mkProcedure..mkFunction] of string =
    ('procedure ', 'function ');
var
  TypeData: PTypeData;
  S: string;
  i, ParamCount: Integer;
  Buf: Pointer;
begin
  if not Assigned(PropInfo^.GetProc) then
    Exit;
  case PropInfo^.PropType^.Kind of
    tkMethod:
      begin
        TypeData := GetTypeData(PropInfo^.PropType);
        if TypeData^.MethodKind in [Low(CMethodKind)..High(CMethodKind)] then
        begin
          S := CMethodKind[TypeData^.MethodKind];
          S := S + PropInfo^.Name;
          Buf := @TypeData^.ParamList;
          ParamCount := TypeData^.ParamCount;
          if ParamCount > 0 then
          begin
            S := S + '(';
            for i := 1 to ParamCount - 1 do
              S := S + TRespalRttiUtils.ExtractParam(Buf) + '; ';
            S := S + TRespalRttiUtils.ExtractParam(Buf) + ')';
            if TypeData^.MethodKind = mkFunction then
              S := S + ': ' + TRespalRttiUtils.ExtractName(Buf);
            S := S + ';';
          end;
          Storage.Append(S);
        end;
      end;
  end;
end;

end.

