unit Common;

interface
uses
  Generics.Collections;

type TDirection = (up, right, down, left, diagUpLeft, diagUpRight, diagDownLeft, diagDownRight);

procedure GetDirectories(out aInputDir, aResultDir: string);
function TryLoadExpectedOutputs(aFilename, aResultDir: string; out aAnswer1, aAnswer2: Integer): Boolean; overload;
function TryLoadExpectedOutputs(aFilename, aResultDir: string; out aAnswer1, aAnswer2: Int64): Boolean; overload;
function TryLoadFile(aFilename: string; out aContents: string): Boolean;

function IntListToString(aList: TList<Integer>): string;
function CheckResult(aActual, aExpected: Integer; aMsg: string = ''): Boolean; overload;
function CheckResult(aActual, aExpected: Int64; aMsg: string = ''): Boolean; overload;

implementation

uses
  System.SysUtils,
  IOUtils;

function TryLoadFile(aFilename: string; out aContents: string): Boolean;
begin
  if FileExists(aFilename) then
    aContents := TFile.ReadAllText(aFilename)
  else
  begin
    aContents := '';
    WriteLn('File not found: ' + aFilename);
  end;

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

function CheckResult(aActual, aExpected: Int64; aMsg: string = ''): Boolean;
begin
  Result := aActual = aExpected;
  WriteLn(Format('Check Result: Expected %d; Actual %d - %s - %s', [aExpected, aActual, BoolToStr(Result, True), aMsg]));
end;

procedure GetDirectories(out aInputDir, aResultDir: string);
begin
  aInputDir := GetCurrentDir + '\inputs\';
  WriteLn('Input Dir: ' + aInputDir);
  aResultDir :=  GetCurrentDir + '\correct\';
  WriteLn('Expected Dir: ' + aResultDir);
end;

function TryLoadExpectedOutputs(aFilename, aResultDir: string; out aAnswer1, aAnswer2: Integer): Boolean;
begin
  TryLoadExpectedOutputs(aFilename, aResultDir, aAnswer1, aAnswer2);
  aAnswer1 := Integer(aAnswer1);
  aAnswer2 := Integer(aAnswer2);
end;

function TryLoadExpectedOutputs(aFilename, aResultDir: string; out aAnswer1, aAnswer2: Int64): Boolean; overload;
begin
  aAnswer1 := -1;
  aAnswer2 := -1;
  var expectedOutputs: string;
  if TryLoadFile(TPath.Combine(aResultDir, aFilename), {o}expectedOutputs) then
  begin
    var answers := expectedOutputs.Split([#$D#$A]);
    aAnswer1 := StrToInt64(answers[0]);
    if Length(answers) > 1 then
      aAnswer2 := StrToInt64(answers[1]);
  end
  else
    WriteLn('File not found: ' + TPath.Combine(aResultDir, aFilename));

  Result := (aAnswer1 <> -1) or (aAnswer2 <> -1);
end;


end.
