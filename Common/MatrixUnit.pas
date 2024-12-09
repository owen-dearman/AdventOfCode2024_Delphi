unit MatrixUnit;

interface
uses Common;

type

TMatrix = TArray<TArray<string>>;
TPos = record
  Row: Integer;
  Col: Integer;
end;

procedure ProcessFileDataIntoMatrix(aContents: string; out aMatrix: TMatrix; out rowCount, colCount: Integer);
function PosIsInMatrix(row, col: Integer; aMatrix: TMatrix): Boolean;

implementation

uses SysUtils;

procedure ProcessFileDataIntoMatrix(aContents: string; out aMatrix: TMatrix; out rowCount, colCount: Integer);
begin
  aMatrix := [];
  var rows := aContents.Split([#$D#$A]);

  for var row in rows do
  begin
    var cols: TArray<string> := [];
    for var char in row do
      cols := cols + [char];
    aMatrix := aMatrix + [cols];
  end;

  rowCount := Length(rows);
  if rowCount > 0 then
    colCount := Length(aMatrix[0])
  else
    colCount := 0;

  //WriteLn('Row Count: ' + rowCount.ToString + '; Column Count: ' + colCount.ToString);
end;

function PosIsInMatrix(row, col: Integer; aMatrix: TMatrix): Boolean;
begin
  var leftBounds := 0;
  var rightBounds := Length(aMatrix[0]) - 1;
  var upBounds := 0;
  var downBounds := Length(aMatrix) - 1;
  Result := (row >= upBounds) and (row <= downBounds) and (col >= leftBounds) and (col <= rightBounds);
end;

end.
