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
    var EXPECTED_OUTPUT_PART_ONE := 0;
    var EXPECTED_OUTPUT_PART_TWO := 0;
    var numSafeLevels := 0;
    var dampNumSafeLevels := 0;

    var inputDir := GetCurrentDir + '\inputs\';
    WriteLn('Input Dir: ' + inputDir);

    var expectedDir :=  GetCurrentDir + '\correct\';
    WriteLn('Expected Dir: ' + expectedDir);

    var testFiles := TDirectory.GetFiles(inputDir, '*.txt');
    var testNum := 1;
    var levels := TStringList.Create;
    try
      for var testFile in testFiles do
      begin

        //Get expected ouputs - assumes answer file has same name as input file
        var expectedOutputFileName := TPath.Combine(expectedDir, ExtractFilename(testFile));
        var expectedOutputs: string;
        if TryLoadFile(expectedOutputFileName, {o}expectedOutputs) then
        begin
          var answers := expectedOutputs.Split([#$D#$A]);
          EXPECTED_OUTPUT_PART_ONE := StrToInt(answers[0]);
          EXPECTED_OUTPUT_PART_TWO := StrToInt(answers[1]);
        end;
        //////////////////////////////////////////////////

        WriteLn(sLineBreak + Format('Test %d: Input File: %s, Answer File: %s', [testNum, testFile, expectedOutputFileName]));
        var fileContents: string;
        if TryLoadFile(testFile, {o}fileContents) and TryProcessInputIntoLevels(fileContents, levels) then
        begin
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
