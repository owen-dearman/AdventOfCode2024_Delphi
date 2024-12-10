program Day7_BridgeRepair;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Common in '..\..\Common\Common.pas',
  Utils in 'Utils.pas',
  IOUtils,
  Generics.Collections;

begin
  try
    var EXPECTED_OUTPUT_PART_ONE, EXPECTED_OUTPUT_PART_TWO: Int64;
    var partOneAnswer: Int64 := 0;
    var partTwoAnswer: Int64 := 0;
    var inputDir, resultDir: string;
    GetDirectories({o}inputDir, {o}resultDir);

    for var testFile in TDirectory.GetFiles(inputDir, '*.txt') do
    begin
      var fileContents: string;
      if TryLoadExpectedOutputs(ExtractFilename(testFile), resultDir, {O}EXPECTED_OUTPUT_PART_ONE, {o}EXPECTED_OUTPUT_PART_TWO) and TryLoadFile(testFile, {o}fileContents) then
      begin
        var dataList := TList<TEquation>.Create;
        ProcessFileData(fileContents, dataList);
        partOneAnswer := CalculateSuccessfulOperations(dataList, False);
        CheckResult(partOneAnswer, EXPECTED_OUTPUT_PART_ONE, 'Total of successful operations');

        partTwoAnswer := CalculateSuccessfulOperations(dataList, True);
        CheckResult(partTwoAnswer, EXPECTED_OUTPUT_PART_TWO, 'Total of successful operations with concat included');
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
