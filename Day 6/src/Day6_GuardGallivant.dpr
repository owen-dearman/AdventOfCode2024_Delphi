program Day6_GuardGallivant;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Common in '..\..\Common\Common.pas',
  Utils in 'Utils.pas',
  IOUtils,
  MatrixUnit in '..\..\Common\MatrixUnit.pas',
  Generics.Collections;

begin
  try
    var EXPECTED_OUTPUT_PART_ONE, EXPECTED_OUTPUT_PART_TWO: Integer;
    var partOneAnswer: Integer;
    var partTwoAnswer := 0;
    var inputDir, resultDir: string;
    GetDirectories({o}inputDir, {o}resultDir);
    for var testFile in TDirectory.GetFiles(inputDir, '*.txt') do
    begin
      var fileContents: string;
      if TryLoadExpectedOutputs(ExtractFilename(testFile), resultDir, {O}EXPECTED_OUTPUT_PART_ONE, {o}EXPECTED_OUTPUT_PART_TWO) and TryLoadFile(testFile, {o}fileContents) then
      begin
        var matrix: TMatrix;
        var rowCount, colCount: Integer;
        var visitedCells := TList<TPos>.Create;
        var _numLoops: Integer;
        ProcessFileDataIntoMatrix(fileContents, {o}matrix, {o}rowCount, {o}colCount);
        ProcessMatrixPath(matrix, visitedCells, {v}_numLoops, {o}partOneAnswer);

        //Part 2
        var matrix2: TMatrix;
        ProcessFileDataIntoMatrix(fileContents, {o}matrix2, {o}rowCount, {o}colCount);
        AddBlockerAndTest(matrix2, fileContents, visitedCells, {o}partTwoAnswer);

        CheckResult(partOneAnswer, EXPECTED_OUTPUT_PART_ONE, 'Number distinct locations reached by guard');
        CheckResult(partTwoAnswer, EXPECTED_OUTPUT_PART_TWO, 'Number of possible loop inducing locations');
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
