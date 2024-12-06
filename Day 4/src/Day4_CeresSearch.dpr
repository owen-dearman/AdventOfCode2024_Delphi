program Day4_CeresSearch;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Utils in 'Utils.pas',
  Common in '..\..\Common\Common.pas',
  IOUtils;

begin
  try
    var EXPECTED_OUTPUT_PART_ONE, EXPECTED_OUTPUT_PART_TWO: Integer;
    var partOneAnswer := 0;
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
        WriteLn('Matrix Created: Rows: ' + rowCount.ToString + '; Columns: ' + colCount.ToString);
        GetCountOfXMAS(matrix, partOneAnswer, partTwoAnswer);
        CheckResult(partOneAnswer, EXPECTED_OUTPUT_PART_ONE, 'How many XMAS in the wordsearch?');
        CheckResult(partTwoAnswer, EXPECTED_OUTPUT_PART_TWO, 'How many X-MAS in the wordsearch?');
        WriteLn('======================================');
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
