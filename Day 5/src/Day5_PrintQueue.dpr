program Day5_PrintQueue;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Common in '..\..\Common\Common.pas',
  Utils in 'Utils.pas',
  IOUtils, Generics.Collections;

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
       var rules := TDictionary<Integer, TArray<TRule>>.Create;
       var updates := TList<TArray<Integer>>.Create;
       var safeUpdates := TList<TArray<Integer>>.Create;
       var unsafeUpdates := TList<TArray<Integer>>.Create;
       try
         ProcessFileData(fileContents, rules, updates);
         CalculateSafeUpdates(rules, updates, safeUpdates, unsafeUpdates);
         partOneAnswer := CountMiddleNumbers(safeUpdates);

         //Prepare for Part 2
         updates.Clear;
         updates.AddRange(unsafeUpdates);
         safeUpdates.Clear;
         unsafeUpdates.Clear;

         FixUnsafeUpdates(updates, rules);
         CalculateSafeUpdates(rules, updates, safeUpdates, unsafeUpdates);
         partTwoAnswer := CountMiddleNumbers(safeUpdates);

         CheckResult(partOneAnswer, EXPECTED_OUTPUT_PART_ONE, 'Middle Value Sum Safe Updates');
         CheckResult(partTwoAnswer, EXPECTED_OUTPUT_PART_TWO, 'Middle Value Sum Unsafe-Now-Safe Updates');
       finally
        rules.Free;
        updates.Free;
        safeUpdates.Free;
        unsafeUpdates.Free;
       end;
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
