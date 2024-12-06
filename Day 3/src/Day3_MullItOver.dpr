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
  try
    var EXPECTED_OUTPUT_PART_ONE, EXPECTED_OUTPUT_PART_TWO: Integer;
    var inputDir, resultDir: string;
    GetDirectories({o}inputDir, {o}resultDir);

    for var testFile in TDirectory.GetFiles(inputDir, '*.txt') do
    begin
      var dataStore := TList<TMulRecord>.Create;
      try
        var fileContents: string;
        if TryLoadExpectedOutputs(ExtractFilename(testFile), resultDir, EXPECTED_OUTPUT_PART_ONE, EXPECTED_OUTPUT_PART_TWO) and TryLoadFile(testFile, fileContents) then
        begin
          ProcessDataIntoMulList(fileContents, dataStore);
          CheckResult(CalculateMultiplicationsAndReturnSumNoSwitches(dataStore), EXPECTED_OUTPUT_PART_ONE, 'Sum of multiplied integers from mul()');

          var disableSwitches := TList<Integer>.Create;
          var enableSwitches := TList<Integer>.Create;
          try
            PopulateIndexesOfSwitches(fileContents, disableSwitches, enableSwitches);
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
