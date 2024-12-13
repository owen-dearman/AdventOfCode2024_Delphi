program Day9_DiskFragmenter;

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
      var diskMap: string;
      if TryLoadExpectedOutputs(ExtractFilename(testFile), resultDir, {O}EXPECTED_OUTPUT_PART_ONE, {o}EXPECTED_OUTPUT_PART_TWO) and TryLoadFile(testFile, {o}diskMap  ) then
      begin
        var fileBlocks := TList<TFileBlock>.Create;
        try
          GenerateIndexes(diskMap, fileBlocks);
          CheckResult(CalculateChecksum(fileBlocks), EXPECTED_OUTPUT_PART_ONE, 'Checksum');
          CheckResult(ManipulateFileBlocks(fileBlocks), EXPECTED_OUTPUT_PART_TWO, 'Checksum Part 2');
        finally
          fileBlocks.Free;
        end;
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
