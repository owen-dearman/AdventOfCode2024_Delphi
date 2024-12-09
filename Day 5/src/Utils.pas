unit Utils;

interface

uses Classes, Generics.Collections;

type
TRelationship = (mustBeBefore, mustBeAfter);

TRule = record
  KeyNum: Integer;
  OtherNum: Integer;
  Relationship: TRelationship;
end;

procedure ProcessFileData(aFileContents: string; aRules: TDictionary<integer, TArray<TRule>>; aUpdates: TList<TArray<Integer>>);
procedure CalculateSafeUpdates(aRules: TDictionary<integer, TArray<TRule>>; aUpdates, aSafeUpdates, aUnsafeUpdates: TList<TArray<Integer>>);
function CountMiddleNumbers(aSafeUpdates: TList<TArray<Integer>>): Integer;
procedure FixUnsafeUpdates(aUpdates: TList<TArray<Integer>>; aRules: TDictionary<integer, TArray<TRule>>);


implementation

uses SysUtils, Generics.Defaults;

procedure ProcessFileData(aFileContents: string; aRules: TDictionary<integer, TArray<TRule>>; aUpdates: TList<TArray<Integer>>);
begin
  var fileSplit := aFileContents.split([#$D#$A#$D#$A]);
  var rulesRaw := fileSplit[0];
  var updatesRaw := fileSplit[1];

  var rules := rulesRaw.Split([#$D#$A]);
  for var rule in rules do
  begin
    var num1 := StrToInt(rule.Split(['|'])[0]);
    var num2 := StrToInt(rule.Split(['|'])[1]);

    var ruleRec: TRule;
    ruleRec.KeyNum := num1;
    ruleRec.OtherNum := num2;
    ruleRec.Relationship := mustBeBefore;

    if aRules.ContainsKey(ruleRec.KeyNum) then
      aRules[ruleRec.KeyNum] := aRules[ruleRec.KeyNum] + [ruleRec]
    else
      aRules.Add(ruleRec.KeyNum, [ruleRec]);

    var ruleRecReverse: TRule;
    ruleRecReverse.KeyNum := num2;
    ruleRecReverse.OtherNum := num1;
    ruleRecReverse.Relationship := mustBeAfter;

    if aRules.ContainsKey(ruleRecReverse.KeyNum) then
      aRules[ruleRecReverse.KeyNum] := aRules[ruleRecReverse.KeyNum] + [ruleRecReverse]
    else
      aRules.Add(ruleRecReverse.KeyNum, [ruleRecReverse]);
  end;

  var updates := updatesRaw.Split([#$D#$A]);
  for var update in updates do
  begin
    var updateList: TArray<Integer> := [];
    var individualNums := update.Split([',']);
    for var num in individualNums do
      updateList := updateList + [StrToInt(num)];
    aUpdates.Add(updateList);
  end;
end;

function AssessRule(aRuleNum: Integer; aUpdate: TArray<Integer>; aRule: TRule): Boolean;
begin
  Result := True;
  Assert(aRule.KeyNum = aUpdate[aRuleNum]);

  var min := 0;
  if aRule.Relationship = mustBeAfter then
    min := aRuleNum;

  var max := aRuleNum;
  if aRule.Relationship = mustBeAfter then
    max := Length(aUpdate) - 1;

  for var i := min to max do
    if (aUpdate[i] = aRule.OtherNum)then
      Exit(False);
end;

function AssessRules(aRuleNum: Integer; aUpdate: TArray<Integer>; aRules: TArray<TRule>): Boolean;
begin
  Result := True;
  for var rule in aRules do
   if not AssessRule(aRuleNum, aUpdate, rule) then
      Exit(False);
end;

function AssessUpdate(aUpdate: TArray<Integer>; aRules: TDictionary<integer, TArray<TRule>>): Boolean;
begin
  Result := True;
  for var i := 0 to Length(aUpdate) - 1 do //Check each number in the update
    if aRules.ContainsKey(aUpdate[i]) and (not AssessRules(i, aUpdate, aRules[aUpdate[i]])) then
      Exit(False);
end;

procedure CalculateSafeUpdates(aRules: TDictionary<integer, TArray<TRule>>; aUpdates, aSafeUpdates, aUnsafeUpdates: TList<TArray<Integer>>);
begin
  for var update in aUpdates do //Check each update
   if AssessUpdate(update, aRules) then
      aSafeUpdates.Add(update)
    else
      aUnsafeUpdates.Add(update);

  WriteLn('Number of Updates: ' + aUpdates.Count.ToString + '; Number of Safe: ' + aSafeUpdates.Count.ToString);
end;

function CountMiddleNumbers(aSafeUpdates: TList<TArray<Integer>>): Integer;
begin
  Result := 0;
  for var update in aSafeUpdates do
    Result := Result + update[Trunc(Length(update) / 2)];
end;

function GetAfterRuleFromNumbers(aNum1, aNum2: Integer; aRules: TDictionary<integer, TArray<TRule>>; aRule: TRule): Boolean;
begin
  Result := False;
  var rules: TArray<TRule>;
  aRules.TryGetValue(aNum1, {O}rules);
  if rules = nil then Exit;

  for aRule in rules do
    if (aRule.OtherNum = aNum2) and (aRule.Relationship = mustBeAfter) then
      Exit(True);
end;

procedure FixUnsafeUpdates(aUpdates: TList<TArray<Integer>>; aRules: TDictionary<integer, TArray<TRule>>);
begin
  var newUpdates := TList<TArray<Integer>>.Create;
  try
    for var update in aUpdates do
    begin
      var testUpdate: TArray<Integer> := update;

      while not AssessUpdate(testUpdate, aRules) do
      begin
        for var i := 0 to Length(testUpdate) -1 do
        begin
          for var j := i + 1 to Length(testUpdate) -1 do
          begin
            var rule: TRule;
            if GetAfterRuleFromNumbers(testUpdate[i], testUpdate[j], aRules, rule) then
            begin
              var num1 := testUpdate[i];
              var num2 := testUpdate[j];
              testUpdate[i] := num2;
              testUpdate[j] := num1;
              Assert(Length(testUpdate) = Length(update));
            end;
          end;
        end;
      end;

      if not AssessUpdate(testUpdate, aRules) then
        WriteLn('Something gone wrong');

      newUpdates.Add(testUpdate);
    end;

    aUpdates.Clear;
    aUpdates.AddRange(newUpdates);
  finally
    newUpdates.Free;
  end;
end;


end.
