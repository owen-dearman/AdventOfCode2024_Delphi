unit Common;

interface
uses
  Generics.Collections;

function TryLoadFile(aFilename: string; out aContents: string): Boolean;
function IntListToString(aList: TList<Integer>): string;

function CheckResult(aActual, aExpected: Integer; aMsg: string = ''): Boolean;

implementation

uses
  System.SysUtils,
  IOUtils;

function TryLoadFile(aFilename: string; out aContents: string): Boolean;
begin
  if FileExists(aFilename) then
    aContents := TFile.ReadAllText(aFilename)
  else
    aContents := '';

  Result := aContents <> '';
end;


function IntListToString(aList: TList<Integer>): string;
begin
  Result := '';
  for var i := 0 to aList.Count - 1 do
  begin
    if i = aList.Count - 1 then
      Result := Result + aList[i].ToString
    else
      Result := Result + aList[i].ToString + ', ';

  end;
end;

function CheckResult(aActual, aExpected: Integer; aMsg: string = ''): Boolean;
begin
  Result := aActual = aExpected;
  WriteLn(Format('Check Result: Expected %d; Actual %d - %s - %s', [aExpected, aActual, BoolToStr(Result, True), aMsg]));
end;


end.
