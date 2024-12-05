unit Utils;

interface

uses
  Generics.Collections;

type
TMulRecord = record
  Int1: Integer;
  Int2: Integer;
  Idx: Integer;
end;

procedure ProcessDataIntoMulList(aText: string; aList: TList<TMulRecord>);
function CalculateMultiplicationsAndReturnSum(aList: TList<TMulRecord>; aDisableList, aEnableList: TList<Integer>): Integer;
function CalculateMultiplicationsAndReturnSumNoSwitches(aList: TList<TMulRecord>): Integer;
procedure GetIndexesOfSwitches(aText: string; aDisableList, aEnableList: TList<Integer>);

implementation

uses
  RegularExpressions,
  SysUtils,
  Math;

function GetRegexMatches(aString: string): TMatchCollection;
begin
  Result := TRegex.Matches(aString, 'mul\([0-9]{1,3},[0-9]{1,3}\)');
end;

//Regex ensures we have "mul(111,22)"
function ExtractIntegersFromMatch(aMatch: TMatch): TMulRecord;
begin
  //,
  var segments := aMatch.Value.split([',']);
  //mul(123
  Result.Int1 := StrToInt(segments[0].Substring(4));
  //123)
  Result.Int2 := StrToInt(segments[1].Substring(0, Length(segments[1]) - 1));
  Result.Idx := aMatch.Index - 1;
end;

procedure PopulateListOfMatchesAsIntegers(aMatches: TMatchCollection; aList: TList<TMulRecord>);
begin
  for var i := 0 to aMatches.Count - 1 do
  begin
    aList.Add(ExtractIntegersFromMatch(aMatches[i]));
  end;
end;

procedure ProcessDataIntoMulList(aText: string; aList: TList<TMulRecord>);
begin
  PopulateListOfMatchesAsIntegers(GetRegexMatches(aText), aList);
end;

function CalculateMultiplicationsAndReturnSum(aList: TList<TMulRecord>; aDisableList, aEnableList: TList<Integer>): Integer;
begin
  Result := 0;
  var useSwitches := (aDisableList <> nil) and (aEnableList <> nil);

  for var mul in aList do
  begin
    var mulEnabled := True;
    if useSwitches then
    begin
      var closestDSwitch := 0;
      var closestESwitch := 0;
      var mulIdx := mul.Idx;
      for var i := 0 to aDisableList.Count - 1 do
        if (aDisableList[i] < mulIdx) then
        begin
          closestDSwitch := aDisableList[i];
        end;

      for var i := 0 to aEnableList.Count - 1 do
        if (aEnableList[i] < mulIdx) then
        begin
          closestESwitch := aEnableList[i];
        end;

      if (mulIDx - closestESwitch) > (mulIdx - closestDSwitch) then
        mulEnabled := False;
    end;

    if (not useSwitches) or mulEnabled then
      Result := Result + (mul.Int1 * mul.Int2);
  end;
end;

function CalculateMultiplicationsAndReturnSumNoSwitches(aList: TList<TMulRecord>): Integer;
begin
  Result := CalculateMultiplicationsAndReturnSum(aList, nil, nil);
end;

procedure GetIndexesOfSwitches(aText: string; aDisableList, aEnableList: TList<Integer>);
begin
  var disabledMatches := TRegex.Matches(aText, 'don''t\(\)');
  for var match in disabledMatches do
    aDisablelist.Add(match.Index - 1);

  var enabledMatches := TRegex.Matches(aText, 'do\(\)');
  for var match in enabledMatches do
    aEnableList.Add(match.Index - 1);
end;

end.
