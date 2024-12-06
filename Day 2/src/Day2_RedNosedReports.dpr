program Day2_RedNosedReports;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  IOUtils,
  Common in '..\..\Common\Common.pas',
  Classes,
  Utils in 'Utils.pas';

begin
  try
    var EXPECTED_OUTPUT_PART_ONE, EXPECTED_OUTPUT_PART_TWO: Integer;
    var numSafeLevels := 0;
    var dampNumSafeLevels := 0;
    var inputDir, resultDir: string;
    GetDirectories({o}inputDir, {o}resultDir);

    var testNum := 1;
    var levels := TStringList.Create;
    try
      for var testFile in TDirectory.GetFiles(inputDir, '*.txt') do
      begin
        var fileContents: string;
        if TryLoadExpectedOutputs(ExtractFilename(testFile), resultDir, EXPECTED_OUTPUT_PART_ONE, EXPECTED_OUTPUT_PART_TWO) and TryLoadFile(testFile, {o}fileContents) and TryProcessInputIntoLevels(fileContents, levels) then
        begin
          WriteLn(sLineBreak + Format('Test %d: Input File: %s, Answer File: %s', [testNum, testFile, TPath.Combine(resultDir, ExtractFilename(testFile))]));
//          WriteLn(sLineBreak + 'Part 1: Without dampening');
          CalculateNumberSafeLevels(levels, {dampen}False, numSafeLevels);

//          WriteLn(sLineBreak + 'Part 2: With dampening');
          CalculateNumberSafeLevels(levels, {dampen}True, dampNumSafeLevels);
        end;

        CheckResult(numSafeLevels, EXPECTED_OUTPUT_PART_ONE, 'Part 1: Number of raw safe levels');
        CheckResult(dampNumSafeLevels, EXPECTED_OUTPUT_PART_TWO, 'Part 2: Number of safe levels including dampened ones');

        levels.Clear;
      end;

    finally
      levels.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  //pause script
  ReadLn;
end.
