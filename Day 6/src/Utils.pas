unit Utils;

interface

uses MatrixUnit, Common, Generics.Collections;

const BASE_POSITION_MARKER = '^';
const CELL_VISITED_MARKER = 'X';
const OBSTACLE_MARKER = '#';
const CELL_UNVISITED_MARKER = '.';

procedure ProcessMatrixPath(aMatrix: TMatrix; aVisitedCells: TList<TPos>; var aNumLoops: Integer; out aCountOfDistinctLocations: Integer);
procedure AddBlockerAndTest(aFileContents: string; aVisitedCells: TList<TPos>; out aNumLoops: Integer);

implementation

uses SysUtils;

procedure GetStartingPosition(aMatrix: TMatrix; out row, col: Integer);
begin
  for var rowNum := 0 to Length(aMatrix) - 1 do
    for var colNum := 0 to Length(aMatrix[rowNum]) - 1 do
      if aMatrix[rowNum][colNum] = BASE_POSITION_MARKER then
      begin
        row := rowNum;
        col := colNum;
        Exit;
      end;
  row := MAXINT;
  col := MAXINT;
end;


function StrIsInt(aStr: string; out aInt: Integer): Boolean;
begin
  try
    aInt := StrToInt(aStr);
    Result := True;
  except
    on e: EConvertError do
      Exit(False);
  end;
end;

procedure ResetPos(var aRow, aCol: Integer; var aDir: TDirection);
begin
  case aDir of
    up:
    begin
     aRow := aRow + 1;
     aDir := left;
    end;
    right:
    begin
      aCol := aCol - 1;
      aDir := up;
    end;
    down:
    begin
      aRow := aRow - 1;
      aDir := right;
    end;
    left:
    begin
      aCol := aCol + 1;
      aDir := down;
    end;
  end;
end;

procedure AdjustPos(var aRow, aCol: Integer; var aDir: TDirection; aMatrix: TMatrix);
begin
  case aDir of
    up:
    begin
      if PosIsInMatrix(aRow - 1, aCol, aMatrix) and (aMatrix[aRow - 1][aCol] = OBSTACLE_MARKER) then
      begin
        aDir := right; //start moving right
        aCol := aCol + 1;
      end
      else
        aRow := aRow - 1; //continue up
    end;
    right:
    begin
      if PosIsInMatrix(aRow, aCol + 1, aMatrix) and (aMatrix[aRow][aCol + 1] = OBSTACLE_MARKER) then
      begin
        aDir := down; //start moving down
        aRow := aRow + 1;
      end
      else
        aCol := aCol + 1; //continue right
    end;
    down:
    begin
      if PosIsInMatrix(aRow + 1, aCol, aMatrix) and (aMatrix[aRow + 1][aCol] = OBSTACLE_MARKER) then
      begin
        aDir := left; //start moving left
        aCol := aCol - 1;
      end
      else
        aRow := aRow + 1; //continue down
    end;
    left:
    begin
      if PosIsInMatrix(aRow, aCol - 1, aMatrix) and (aMatrix[aRow][aCol - 1] = OBSTACLE_MARKER) then
      begin
        aDir := up; //start moving up
        aRow := aRow - 1;
      end
      else
        aCol := aCol - 1; //continue down
    end;
  end;
end;

procedure ProcessMatrixPath(aMatrix: TMatrix; aVisitedCells: TList<TPos>; var aNumLoops: Integer; out aCountOfDistinctLocations: Integer);
begin
  aCountOfDistinctLocations := 0;
  var row, col: Integer;
  GetStartingPosition(aMatrix, {o}row, {o}col);
  Assert((row <> MAXINT) and (col <> MAXINT));

  //Guard starts by moving up
  var movementDir := up;

  //convert guard ^ into unvisited marker so it is counted
  aMatrix[row][col] := CELL_UNVISITED_MARKER;

  while PosIsInMatrix(row, col, aMatrix) do
  begin
    //If guard is on a square he's not been on before, count as distinct and log the position for later
    if (aMatrix[row][col] = CELL_UNVISITED_MARKER) then
    begin
      Inc(aCountOfDistinctLocations);
      //number of time stepped on is 0 as we'll increment it later
      aMatrix[row][col] := '0';
      if aVisitedCells <> nil then
      begin
        var pos: TPos;
        pos.Row := row;
        pos.Col := col;
        aVisitedCells.Add(pos);
      end;
    end;

    //For multiple visits to same spot
    var numVisits: Integer;
    if StrIsInt(aMatrix[row][col], {o}numVisits) then
    begin
      if numVisits = 5 then
      begin
        Inc(aNumLoops);
        Exit; //We deem 5 visits to the same spot to be in a loop
      end;
      aMatrix[row][col] := IntToStr(numVisits + 1);
    end;

    //Step forward, or turn and step if in front of an obstacle
    AdjustPos(row, col, movementDir, aMatrix);

    //If you've found yourself on an obstacle (part 2), then go back a step and try the next direction instead. If you've
    //stepped out of bounds, then don't do this (part 1)
    while PosIsInMatrix(row, col, aMatrix) and (aMatrix[row][col] = OBSTACLE_MARKER) do
    begin
      var failedDir := movementDir;
      ResetPos(row, col, movementDir);

      case failedDir of
        up: movementDir := right;
        right: movementDir := down;
        down: movementDir := left;
        left: movementDir := up;
      end;
      AdjustPos(row, col, movementDir, aMatrix);
    end;
  end;
end;

procedure AddBlockerAndTest(aFileContents: string; aVisitedCells: TList<TPos>; out aNumLoops: Integer);
begin
  var _count: Integer;
  var rowCount, colCount: Integer;
  aNumLoops := 0;
  WriteLn('Starting Analysis...');
  for var cell in aVisitedCells do //Only check positions that the Part 1 route visited
  begin
    var testMatrix: TMatrix;
    ProcessFileDataIntoMatrix(aFileContents, testMatrix, rowCount, colCount); //Reset the matrix
    if testMatrix[cell.Row][cell.Col] <> CELL_UNVISITED_MARKER then //Can't place obstacle where guard starts
        Continue;

    testMatrix[cell.Row][cell.Col] := OBSTACLE_MARKER;  //Set the obstacle
    ProcessMatrixPath(testMatrix, nil, {v}aNumLoops, {o}_count);
  end;
end;

end.
