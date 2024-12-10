unit Utils;

interface

uses SysUtils, Generics.Collections;

type

TOperation = (multiply, add, concat);

TEquation = record
  Total: Int64;
  Components: TArray<Int64>;
  Operations: TArray<TOperation>;
  function ToStr: string;
end;

procedure ProcessFileData(aFileContents: string; aList: TList<TEquation>);
function CalculateSuccessfulOperations(var aList: TList<TEquation>; aUSeConcat: Boolean): Int64;

implementation

uses Math;

procedure ProcessFileData(aFileContents: string; aList: TList<TEquation>);
begin
  var lines := aFileContents.Split([#$D#$A]);

  for var line in lines do
  begin
    var segments := line.Split([': ']);
    var equation: TEquation;
    equation.Total := StrToInt64(segments[0]);
    equation.Components := [];

    for var num in segments[1].Split([' ']) do
      equation.Components := equation.Components + [StrToInt64(num)];

    aList.Add(equation);
  end;
end;

function CalculationPasses(aEq: TEquation): Boolean;
begin
  Assert(Length(aEq.Components) - Length(aEq.Operations) = 1);
  var total: Int64 := aEq.Components[0];
  for var i := 1 to Length(aEq.Components) - 1 do
  begin
    case aEq.Operations[i - 1] of
      add: total := total + aEq.Components[i];
      multiply: total := total * aEq.Components[i];
      concat: total := StrToInt64(IntToStr(total) + IntToStr(aEq.Components[i]));
    end;
  end;
  Result := total = aEq.Total;
end;

procedure PopulateCombinations(aOrigList: TEquation; aCombos: TList<TArray<TOperation>>);
begin
  var totalCombinations := 1 shl Length(aOrigList.Components);
  for var i := 0 to totalCombinations do
  begin
    var combination: TArray<TOperation> := [];
    for var j := 0 to Length(aOrigList.Components) - 2 do
    begin
      if (i shr j) and 1 = 1 then
        combination := [add] + combination
      else
        combination := [multiply] + combination;
    end;
    aCombos.Add(combination);
  end;
end;

procedure PopulateCombinationsWithThirdOption(aOrigList: TEquation; aCombos: TList<TArray<TOperation>>);
begin
  var totalCombinations := Trunc(Power(3, Length(aOrigList.Components) - 1));

  for var i := 0 to totalCombinations - 1 do
  begin
    var combination: TArray<TOperation> := [];
    for var j := 0 to Length(aOrigList.Components) - 2 do
    begin
      case (i div Trunc(Power(3, j))) mod 3 of
        0: combination := [add] + combination;
        1: combination := [multiply] + combination;
        2: combination := [concat] + combination;
      end;
    end;
    aCombos.Add(combination);
  end;
end;

function FindSuccessfulOperations(var aEq: TEquation; aUseThirdOption: Boolean): Boolean;
begin
  var combos := TList<TArray<TOperation>>.Create;
  try
    if not aUseThirdOption then
      PopulateCombinations(aEq, combos)
    else
      PopulateCombinationsWithThirdOption(aEq, combos);

    for var combo in combos do
    begin
      aEq.Operations := combo;
      if CalculationPasses(aEq) then
        Exit(True);
    end;
    Result := False;
  finally
    combos.Free;
  end;
end;

function CalculateSuccessfulOperations(var aList: TList<TEquation>; aUSeConcat: Boolean): Int64;
begin
  Result := 0;
  for var i := 0 to aList.Count - 1 do
  begin
    var eq := aList[i];
    if FindSuccessfulOperations(eq, aUseConcat) then
      Result := Result + eq.Total
    else
      WriteLn('Unsuccessful: ' + eq.ToStr);
  end;
end;

{ TEquation }

function TEquation.ToStr: string;
begin
  Result := Total.ToString;
  for var num in Components do
   Result := Result +  ', ' + num.ToString;
end;

end.
