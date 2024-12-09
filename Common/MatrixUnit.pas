unit MatrixUnit;

interface

type
TMatrix = TArray<TArray<string>>;

procedure ProcessFileDataIntoMatrix(aContents: string; out aMatrix: TMatrix; out rowCount, colCount: Integer);

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

  WriteLn('Row Count: ' + rowCount.ToString + '; Column Count: ' + colCount.ToString);
end;

end.
