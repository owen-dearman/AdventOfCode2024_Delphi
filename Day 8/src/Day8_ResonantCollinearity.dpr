program Day8_ResonantCollinearity;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Common in '..\..\Common\Common.pas',
  MatrixUnit in '..\..\Common\MatrixUnit.pas',
  Utils in 'Utils.pas',
  IOUtils, Generics.Collections;



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
        ProcessFileDataIntoMatrix(fileContents, {o}matrix, {o}rowCount, {o}colCount);

        partOneAnswer := CountAntinodes(matrix, False);
        CheckResult(partOneAnswer, EXPECTED_OUTPUT_PART_ONE, 'Number of antinode positions');

         WriteLn;

        partTwoAnswer := CountAntinodes(matrix, True);
        CheckResult(partTwoAnswer, EXPECTED_OUTPUT_PART_TWO, 'Number of antinode positions including resonant frequencies');

        WriteLn;
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
