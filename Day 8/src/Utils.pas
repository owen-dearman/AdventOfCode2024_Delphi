unit Utils;

interface

uses MatrixUnit;

function CountAntinodes(const matrix: TArray<TArray<string>>; aUseResonantFreq: Boolean): Integer;

implementation

uses Generics.Collections, SysUtils;

procedure PopulateAntennaePoints(aAntDict: TDictionary<string, TArray<TPoint>>; aMatrix: TMatrix);
begin
  aAntDict.Clear;
  for var row := 0 to High(aMatrix) do
    for var col := 0 to High(aMatrix[row]) do
    begin
      var pos := aMatrix[row][col];
      if pos <> '.' then
        if aAntDict.ContainsKey(pos) then
          aAntDict[pos] := aAntDict[pos] + [TPoint.Create(row, col)]
        else
          aAntDict.Add(pos, [TPoint.Create(row, col)]);
    end;
end;

procedure TryFindProjections(aMatrix: TMatrix; aAntennaPos, aAntennaPos2, aDiff : TPoint; aAntinodes: TList<TPoint>; aUseResonantFreq: Boolean);
begin
  var proj := aAntennaPos.Project(aDiff);
  while PosIsInMatrix(proj.Y, proj.X, aMatrix) do
  begin
    if (aUseResonantFreq or proj.IsDoubleDistanceToOnePoint(aAntennaPos, aAntennaPos2)) and (not aAntinodes.Contains(proj)) then
      aAntinodes.Add(proj);
    if not aUseResonantFreq then
      Exit;

    proj := proj.Project(aDiff);
  end;
end;

procedure FindPositionsOfAntinodes(aMatrix: TMatrix; aAntDict: TDictionary<string, TArray<TPoint>>; aAntinodes: TList<TPoint>; aUseResonantFreq: Boolean);
begin
  for var ant in aAntDict do
    for var i := 0 to High(ant.Value) do
      for var j := 0 to High(ant.Value) do
      begin
        var diff := ant.Value[i].Diff(ant.Value[j]);
        if (diff.Y = 0) and (diff.X = 0) then Continue;

        TryFindProjections(aMatrix, ant.Value[i], ant.Value[j], diff, aAntinodes, aUseResonantFreq);
        if not aUseResonantFreq then
            Continue;

        //Also have to look "inwards" from each antenna when finding resonant frequencies
        TryFindProjections(aMatrix, ant.Value[i], ant.Value[j], TPoint.Create(diff.Y * -1, diff.X * -1), aAntinodes, True);
      end;
end;

procedure Visualise(aMatrix: TMatrix; aAntinodes: TList<TPoint>);
begin
  for var i := 0 to Length(aMatrix) - 1 do
  begin
    var line := '';
    for var j := 0 to Length(aMatrix[i]) - 1 do
      if aAntinodes.Contains(TPoint.Create(i, j)) then
        line := line + '#'
      else
        line := line + aMatrix[i][j];
    WriteLn(line);
  end;
end;

function CountAntinodes(const MATRIX: TMatrix; aUseResonantFreq: Boolean): Integer;
begin
  var antennae := TDictionary<string, TArray<TPoint>>.Create;
  var antinodes := TList<TPoint>.Create;
  try
    PopulateAntennaePoints(antennae, MATRIX);
    FindPositionsOfAntinodes(MATRIX, antennae, antinodes, aUseResonantFreq);

    Visualise(MATRIX, antinodes);
    Result := antinodes.Count;
  finally
    antennae.Free;
    antinodes.Free;
  end;
end;


end.
