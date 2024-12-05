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
    var EXPECTED_OUTPUT_PART_ONE := 0;
    var EXPECTED_OUTPUT_PART_TWO := 0;
    var listDistance := 0;
    var listSimilarity := 0;

    var inputDir := GetCurrentDir + '\inputs\';
    WriteLn('Input Dir: ' + inputDir);
    var expectedDir :=  GetCurrentDir + '\correct\';
    WriteLn('Expected Dir: ' + expectedDir);

    var testFiles := TDirectory.GetFiles(inputDir, '*.txt');
    var testNum := 1;
    var listOne := TList<Integer>.Create;
    var listTwo := TList<Integer>.Create;
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


        WriteLn(sLineBreak + Format('Test %d: Input File: %s; Answer File: %s', [testNum, testFile, expectedOutputFileName]));
        var fileContents: string;
        if TryLoadFile(testFile, {o}fileContents) and TryProcessFileDataIntoTwoLists(fileContents, listOne, listTwo) then
        begin
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
