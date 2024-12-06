program Day1_HistorianHysteria;

{$APPTYPE CONSOLE}

{$R *.res}
uses
  System.SysUtils,
  IOUtils,
  Generics.Collections,
  Common in '..\..\Common\Common.pas',
  Utils in 'Utils.pas';

begin
  try
    var EXPECTED_OUTPUT_PART_ONE, EXPECTED_OUTPUT_PART_TWO: Integer;
    var listDistance := 0;
    var listSimilarity := 0;
    var inputDir, resultDir: string;
    GetDirectories({o}inputDir, {o}resultDir);

    var testNum := 1;
    var listOne := TList<Integer>.Create;
    var listTwo := TList<Integer>.Create;
    try
      for var testFile in TDirectory.GetFiles(inputDir, '*.txt') do
      begin
        var fileContents: string;
        if TryLoadExpectedOutputs(ExtractFilename(testFile), resultDir, EXPECTED_OUTPUT_PART_ONE, EXPECTED_OUTPUT_PART_TWO) and TryLoadFile(testFile, {o}fileContents) and TryProcessFileDataIntoTwoLists(fileContents, listOne, listTwo) then
        begin
          WriteLn(sLineBreak + Format('Test %d: Input File: %s; Answer File: %s', [testNum, testFile, TPath.Combine(resultDir, ExtractFilename(testFile))]));
          //sort smallest to largest
          listOne.Sort;
          listTwo.Sort;

          //calcualte "distance" between lists
          listDistance := CalculateDistanceBetweenLists(listOne, listTwo);

          //calculate similarity
          listSimilarity := CalculateSimilarityBetweenLists(listOne, listTwo);
        end;

        CheckResult(listDistance, EXPECTED_OUTPUT_PART_ONE, 'Distance between lists');
        CheckResult(listSimilarity, EXPECTED_OUTPUT_PART_TWO, 'Similarity between lists');

        Inc(testNum);
        listOne.Clear;
        listTwo.Clear;
      end;
    finally
      listOne.Free;
      listTwo.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  //pause script
  ReadLn;
end.
