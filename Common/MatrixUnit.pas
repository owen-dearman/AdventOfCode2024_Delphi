unit MatrixUnit;

interface
uses Common;

type

TMatrix = TArray<TArray<string>>;

//REMEMBER ROW=Y AND COLUMN=X
TPoint = record
  X, Y: Integer;
  constructor Create(aY, aX: Integer);
  function Equals(const Other: TPoint): Boolean;
  function Diff(const Other: TPoint): TPoint;
  function Project(const aProjection: TPoint): TPoint;
  function IsDoubleDistanceToOnePoint(aPointA, aPointB: TPoint): Boolean;
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

  //WriteLn('Y Count: ' + rowCount.ToString + '; Xumn Count: ' + colCount.ToString);
end;

function PosIsInMatrix(row, col: Integer; aMatrix: TMatrix): Boolean;
begin
  var leftBounds := 0;
  var rightBounds := Length(aMatrix[0]) - 1;
  var upBounds := 0;
  var downBounds := Length(aMatrix) - 1;
  Result := (row >= upBounds) and (row <= downBounds) and (col >= leftBounds) and (col <= rightBounds);
end;

{TPoint}
constructor TPoint.Create(aY, aX: Integer);
begin
  Y := aY;
  X := aX;
end;

function TPoint.Diff(const Other: TPoint): TPoint;
begin
  Result := TPoint.Create(Y - Other.Y, X - Other.X);
end;

function TPoint.Equals(const Other: TPoint): Boolean;
begin
  Result := (Y = Other.Y) and (X = Other.X);
end;

function TPoint.IsDoubleDistanceToOnePoint(aPointA, aPointB: TPoint): Boolean;
begin
  var diff := Self.Diff(aPointA);
  var diff2 := Self.Diff(aPointB);

  Result := ((diff.Y / diff2.Y = 2) and (diff.X / diff2.X = 2)) or ((diff2.Y / diff.Y = 2) and (diff2.X / diff.X = 2));
end;

function TPoint.Project(const aProjection: TPoint): TPoint;
begin
  Result := TPoint.Create(Y + aProjection.Y, X + aProjection.X);
end;

end.
