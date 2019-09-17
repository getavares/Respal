unit Respal.Regex.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TRespalRegexUtils }

  TRespalRegexUtils = class
  public
    class function ExprWordsInText(AWords: array of String; AOcurrences: Integer;
      AFromInit: Boolean; AUntilEnd: Boolean): String;
    class function ExprNamedRoutesParams(ATypes: array of String): String;
  end;


implementation

{ TRespalRegexUtils }

class function TRespalRegexUtils.ExprWordsInText(AWords: array of String;
  AOcurrences: Integer; AFromInit: Boolean; AUntilEnd: Boolean): String;
var
  I: Integer;
  LWords: String;
begin
if AFromInit then
  Result:='^';
LWords:='';
for I:=Low(AWords) to High(AWords) do
  begin
  if LWords<>'' then
    LWords:=LWords+'|';
  LWords:=LWords+AWords[I];
  end;
Result:=Result+'('+LWords+'){'+IntToStr(AOcurrences)+'}';
if AUntilEnd then
  Result:=Result+'$';
end;

class function TRespalRegexUtils.ExprNamedRoutesParams(ATypes: array of String
  ): String;
const
  NamedParam = '\<{1}(\w*)\:%s\>';
var
  LParams: String;
  I: Integer;
begin
LParams:='';
for I:=Low(ATypes) to High(ATypes) do
  begin
  if LParams<>'' then
    LParams:=LParams+'|';
  LParams:=LParams+Format(NamedParam, [ATypes[I]]);
  end;
Result:='^('+LParams+')$';
end;

end.

