unit Utils;

interface
uses
  Generics.Collections;

function TryProcessFileDataIntoTwoLists(aFileData: string; aListOne, aListTwo: TList<Integer>): Boolean;
function CalculateDistanceBetweenLists(aListOne, aListTwo: TList<Integer>): Integer;
function CalculateSimilarityBetweenLists(aListOne, aListTwo: TList<Integer>): Integer;

implementation
uses
  Classes,
  SysUtils;

function TryProcessFileDataIntoTwoLists(aFileData: string; aListOne, aListTwo: TList<Integer>): Boolean;
begin
  //Split into "rows" on line break & carrriage return
  Result := aFileData <> '';

  if not Result then
    Raise Exception.Create('File data blank');

  var rows := aFileData.Split([#$D#$A]);
  for var row in rows do
  begin
    //split columns on three spaces
    var cols := row.Split(['   ']);
    if Length(cols) = 1 then
      aListOne.Add(StrToInt(cols[0]))
    else if Length(cols) = 2 then
    begin
      aListOne.Add(StrToInt(cols[0]));
      aListTwo.Add(StrToInt(cols[1]));
    end
    else
    begin
      Result := False;
      Raise Exception.Create('Error parsing columns: ' + row);
    end;
  end;

//  WriteLn(Format('List One Count: %d, List Two Count: %d', [aListOne.Count, aListTwo.Count]));
end;

function CalculateDistanceBetweenLists(aListOne, aListTwo: TList<Integer>): Integer;
begin
  Result := 0;
  for var i := 0 to aListOne.Count-1 do
  begin
    var numOne := aListOne[i];
    if i >= aListTwo.Count then
      Exit;

    Result := Result + Abs(numOne - aListTwo[i]);
  end;
end;

function CalculateSimilarityBetweenLists(aListOne, aListTwo: TList<Integer>): Integer;
begin
  Result := 0;
  for var num1 in aListOne do
  begin
    var occInListTwo := 0;

    for var num2 in aListTwo do
      if num1 = num2 then
        Inc(occInListTwo);

    Result := Result + (num1 * occInListTwo);
  end;
end;

end.
