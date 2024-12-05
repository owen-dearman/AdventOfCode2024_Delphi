unit Utils;

interface
uses
  Classes,
  SysUtils,
  Generics.Collections,
  Common;

 type TSequence = (sAsc, sDesc, sUnk);
 const SEQUENCE_STR: array[TSequence] of string = ('ASCENDING', 'DESCENDING', 'UNKNOWN');

function TryProcessInputIntoLevels(aInput: string; aLevels: TStringList): Boolean;
function IsLevelSafe(aLevel: string; aUseDampener: Boolean): Boolean; overload;
function IsLevelSafe(aList: TList<Integer>): Boolean; overload;
procedure CalculateNumberSafeLevels(aLevels: TStringList; aDampen: Boolean; var safeNum: Integer);

implementation

uses
  StrUtils;

function TryProcessInputIntoLevels(aInput: string; aLevels: TStringList): Boolean;
begin
  aLevels.AddStrings(aInput.Split([#$D#$A]));
end;

function IsStillInSequence(var aSeq: TSequence; aCurr, aNext: Integer): Boolean;
begin
  case aSeq of
    sUnk:
    begin
      if aNext > aCurr then
        aSeq := sAsc
      else if aNext < aCurr then
        aSeq := sDesc
      else
        Exit(False);
    end;
    sAsc:
    begin
      if aNext <= aCurr then
        Exit(False);
    end;
    sDesc:
    begin
      if aNext >= aCurr then
        Exit(False);
    end;
  end;
  Result := True;
end;

{
The levels are either all increasing or all decreasing.
Any two adjacent levels differ by at least one and at most three.

Dampener allows for one bad level to allow the report to still be safe
}

function IsLevelSafe(aLevel: string; aUseDampener: Boolean): Boolean;
var
  failIdx1: Integer;
  failIdx2: Integer;
begin
  var reportList := TList<Integer>.Create;
  try
//    WriteLn('====================');

    var reports := aLevel.Split([' ']);
    for var report in reports do
      reportList.Add(StrToInt(report));

    Result := IsLevelSafe(reportList);

    if Result or (not aUseDampener) then
      Exit;

    for var j := 0 to Length(reports) - 1 do
    begin
      reportList.Clear;
      for var i := 0 to Length(reports) - 1 do
        if i <> j then
          reportList.Add(StrToInt(reports[i]))
        else
        begin
//          WriteLn('Attempting Dampener. Removing idx: ' + j.ToString + '; ' + reports[i] + ' from list');
        end;

        Result := IsLevelSafe(reportList);
        if Result then Exit;
    end;

  finally
    reportList.Free;
  end;
end;

function IsLevelSafe(aList: TList<Integer>): Boolean;
begin
  var unsafeReason := '';
  Result := True;
  try
    var sequence := sUnk;
    for var i := 0 to aList.Count - 1 do
    begin
      var currRep := aList[i];

      if i + 1 > aList.Count - 1 then
        Break;

      var nextRep := aList[i+1];

      var absoluteDiff := Abs(currRep - nextRep);
      if not ((absoluteDiff > 0) and (absoluteDiff < 4)) then
      begin
        unsafeReason := Format('Level Difference Broken: Current Report: %d; Next Report: %d; Difference: %d', [currRep, nextRep, absoluteDiff]);
        Exit(False);
      end;

      if not IsStillInSequence(sequence, currRep, nextRep) then
      begin
        unsafeReason := Format('Sequence Broken: %s; Current Report: %d; Next Report %d', [SEQUENCE_STR[sequence], currRep, nextRep]);
        Exit(False);
      end;
    end;

  finally
//    WriteLn(IfThen(Result, 'Safe', 'Unsafe') + ': ' + IntListToString(aList) + IfThen(unsafeReason <> '', ' - ' + unsafeReason, ''));
  end;
end;


procedure CalculateNumberSafeLevels(aLevels: TStringList; aDampen: Boolean; var safeNum: Integer);
begin
  for var level in aLevels do
    if IsLevelSafe(level, aDampen{useDampener}) then
      Inc(safeNum);
end;

end.
