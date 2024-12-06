unit Common;

interface
uses
  Generics.Collections;

procedure GetDirectories(out aInputDir, aResultDir: string);
function TryLoadExpectedOutputs(aFilename, aResultDir: string; out aAnswer1, aAnswer2: Integer): Boolean;
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

procedure GetDirectories(out aInputDir, aResultDir: string);
begin
  aInputDir := GetCurrentDir + '\inputs\';
  WriteLn('Input Dir: ' + aInputDir);
  aResultDir :=  GetCurrentDir + '\correct\';
  WriteLn('Expected Dir: ' + aResultDir);
end;

function TryLoadExpectedOutputs(aFilename, aResultDir: string; out aAnswer1, aAnswer2: Integer): Boolean;
begin
  aAnswer1 := MAXINT;
  aAnswer2 := MAXINT;
  var expectedOutputs: string;
  if TryLoadFile(TPath.Combine(aResultDir, aFilename), {o}expectedOutputs) then
  begin
    var answers := expectedOutputs.Split([#$D#$A]);
    aAnswer1 := StrToInt(answers[0]);
    if Length(answers) > 1 then
      aAnswer2 := StrToInt(answers[1]);
  end
  else
    WriteLn('File not found: ' + TPath.Combine(aResultDir, aFilename));

  Result := (aAnswer1 < MAXINT) or (aAnswer2 < MAXINT);
end;


end.
