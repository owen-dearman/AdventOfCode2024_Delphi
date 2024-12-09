unit Utils;

interface

uses MatrixUnit, Common;

procedure GetCountOfXMAS(aMatrix: TMatrix; out aXmasCount, aX_masCount: Integer);
const theWord = 'XMAS';

implementation

uses SysUtils;

procedure AdjustMatrixCell(var aRow, aCol: Integer; const aOffset: Integer; const aDirection: TDirection);
begin
  case aDirection of
    up:
    begin
      aRow := aRow - aOffset;
    end;
    down:
    begin
      aRow := aRow + aOffset;
    end;
    left:
    begin
      aCol := aCol - aOffset;
    end;
    right:
    begin
      aCol := aCol + aOffset;
    end;
    diagUpRight:
    begin
      aRow := aRow - aOffset;
      aCol := aCol + aOffset;
    end;
    diagUpLeft:
    begin
      aRow := aRow - aOffset;
      aCol := aCol - aOffset;
    end;
    diagDownRight:
    begin
      aRow := aRow + aOffset;
      aCol := aCol + aOffset;
    end;
    diagDownLeft:
    begin
      aRow := aRow + aOffset;
      aCol := aCol - aOffset;
    end;
  end;
end;

procedure FindAllXmasAtPoint(const aWordToFind: string; aX, aY: Integer; var aTotal: Integer; aMatrix: TMatrix);
begin
  var currentTotal := aTotal;
  var rowUpBounds := 0;
  var rowDownBounds := Length(aMatrix) - 1;
  var colLeftBounds := 0;
  var colRightBounds := Length(aMatrix[0]) - 1;

  for var i := 0 to 7 do  //for each direction
  begin
    var dir := TDirection(i);
    var builtWord := '';
    for var j := 0 to Length(aWordToFind) - 1 do //for each letter
    begin
      var row := aX;
      var col := aY;
      var wantedLetter := aWordToFind[j + 1];

      AdjustMatrixCell({v}row, {v}col, {c}j, {c}dir);

      if (row < rowUpBounds) or (row > rowDownBounds) or (col < colLeftBounds) or (col > colRightBounds) then
        Break;
      var letter := Uppercase(aMatrix[row][col]);
      if letter <> wantedLetter then
        Break;
      builtWord := builtWord + letter;
    end;

    if (builtWord = aWordToFind) then //this may be less than 4 letters so have to check
      Inc(aTotal);
  end;

//  WriteLn(Format('Found %d XMAS from this X',[aTotal - currentTotal]))
end;

procedure FindX_masAtPoint(aX, aY: Integer; var aTotal: Integer; aMatrix: TMatrix);
begin
  Assert(aMatrix[aX][aY] = 'A');

  var rowUpBounds := 0;
  var rowDownBounds := Length(aMatrix) - 1;
  var colLeftBounds := 0;
  var colRightBounds := Length(aMatrix[0]) - 1;

  //diagUpLeft, diagUpRight, diagDownLeft, diagDownRight
  var lettersFoundMatchingDir := '';

  for var i := 0 to 3 do
  begin
    var row := aX;
    var col := aY;
    var dir := TDirection(i + 4); //only care about diagonals now
    AdjustMatrixCell({v}row, {v}col, {c}1, {c}dir);
    if (row < rowUpBounds) or (row > rowDownBounds) or (col < colLeftBounds) or (col > colRightBounds) then
      Exit;

    var letter := aMatrix[row][col];
    lettersFoundMatchingDir := lettersFoundMatchingDir + letter;
  end;

  if Length(lettersFoundMatchingDir) <> 4 then
    Exit;

  var correctWords := ['MMSS', 'MSMS', 'SSMM', 'SMSM'];   //Working clockwise from top left
  for var word in correctWords do
    if word = lettersFoundMatchingDir then
    begin
      Inc(aTotal);
      Break;
    end;
end;

procedure GetCountOfXMAS(aMatrix: TMatrix; out aXmasCount, aX_masCount: Integer);
begin
  aXmasCount := 0;
  aX_masCount := 0;

  for var x := 0 to Length(aMatrix) - 1 do
    for var y := 0 to Length(aMatrix[x]) - 1 do
    begin
      var cell := aMatrix[x][y];
      if Uppercase(cell) = theWord[1] then //X
      begin
//        WriteLn(Format('X found at coordinates (%d,%d)',[y,x]));
        FindAllXmasAtPoint(Uppercase(theWord), x, y, aXmasCount, aMatrix);
      end;
      if Uppercase(cell) = theWord[3] then //A
      begin
      //        WriteLn(Format('A found at coordinates (%d,%d)',[y,x]));
        FindX_masAtPoint(x, y, aX_masCount, aMatrix);
      end;
    end;
end;

end.
