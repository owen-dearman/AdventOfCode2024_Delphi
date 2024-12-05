program Day3_MullItOver;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Generics.Collections,
  IOUtils,
  Common in '..\..\Common\Common.pas',
  Utils in 'Utils.pas';

begin

  var EXPECTED_OUTPUT_PART_ONE := 0;
  var EXPECTED_OUTPUT_PART_TWO := 0;
  try
    var inputDir := GetCurrentDir + '\inputs\';
    WriteLn('Input Dir: ' + inputDir);
    var expectedDir :=  GetCurrentDir + '\correct\';
    WriteLn('Expected Dir: ' + expectedDir);

    var testFiles := TDirectory.GetFiles(inputDir, '*.txt');

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

      var dataStore := TList<TMulRecord>.Create;
      try
        var fileContents: string;
        if TryLoadFile(testFile, fileContents) then
        begin
          ProcessDataIntoMulList(fileContents, dataStore);
          CheckResult(CalculateMultiplicationsAndReturnSumNoSwitches(dataStore), EXPECTED_OUTPUT_PART_ONE, 'Sum of multiplied integers from mul()');

          var disableSwitches := TList<Integer>.Create;
          var enableSwitches := TList<Integer>.Create;
          try
            GetIndexesOfSwitches(fileContents, disableSwitches, enableSwitches);
            CheckResult(CalculateMultiplicationsAndReturnSum(dataStore, disableSwitches, enableSwitches), EXPECTED_OUTPUT_PART_TWO, 'Sum of enabled multiplied integers from mul()');
          finally
            disableSwitches.Free;
            enableSwitches.Free;
          end;
        end;
      finally
        dataStore.Free;
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  ReadLn;
end.
