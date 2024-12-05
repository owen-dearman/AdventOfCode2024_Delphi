program Day1_HistorianHysteria;

{$APPTYPE CONSOLE}

{$R *.res}
uses
  System.SysUtils,
  IOUtils,
  Generics.Collections;

function LoadInputFile(aFilename: string): string;
  begin
    if FileExists(aFilename) then
      Result := TFile.ReadAllText(aFilename)
    else
      Result := '';
  end;

  function TryProcessFileDataIntoTwoLists(aFileData: string; aListOne, aListTwo: TList<Integer>): Boolean;
  begin
    //Split into "rows" on line break & carrriage return
    Result := aFileData <> '';

    if not Result then
      Raise Exception.Create('File data blank');

    var rows := aFileData.Split([#$D#$A]);
    for var row in rows do
    begin
      //split columns on three spaces
      var cols := row.Split(['   ']);
      if Length(cols) = 1 then
        aListOne.Add(StrToInt(cols[0]))
      else if Length(cols) = 2 then
      begin
        aListOne.Add(StrToInt(cols[0]));
        aListTwo.Add(StrToInt(cols[1]));
      end
      else
      begin
        Result := False;
        Raise Exception.Create('Error parsing columns: ' + row);
      end;
    end;

    WriteLn(Format('List One Count: %d, List Two Count: %d', [aListOne.Count, aListTwo.Count]));
  end;

  function CalculateDistanceBetweenLists(aListOne, aListTwo: TList<Integer>): Integer;
  begin
    Result := 0;
    for var i := 0 to aListOne.Count-1 do
    begin
      var numOne := aListOne[i];
      if i >= aListTwo.Count then
        Exit;

      Result := Result + Abs(numOne - aListTwo[i]);
    end;
  end;

  function CalculateSimilarityBetweenLists(aListOne, aListTwo: TList<Integer>): Integer;
  begin
    Result := 0;
    for var num1 in aListOne do
    begin
      var occInListTwo := 0;

      for var num2 in aListTwo do
        if num1 = num2 then
          Inc(occInListTwo);

      Result := Result + (num1 * occInListTwo);
    end;
  end;

begin
  try
    var inputDir := GetCurrentDir + '\inputs\';
    var testFiles := TDirectory.GetFiles(inputDir, '*.txt');
    var testNum := 1;

    WriteLn('Input Dir: ' + inputDir);

    var listOne := TList<Integer>.Create;
    var listTwo := TList<Integer>.Create;
    try
      for var testFile in testFiles do
      begin
        WriteLn(Format('Test %d: Input File %s', [testNum, testFile]));

        if TryProcessFileDataIntoTwoLists(LoadInputFile(testFile), listOne, listTwo) then
        begin
          //sort smallest to largest
          listOne.Sort;
          listTwo.Sort;

          //calcualte "distance" between lists
          var listDistance := CalculateDistanceBetweenLists(listOne, listTwo);
          WriteLn('Total List Difference: ' + listDistance.ToString);

          //calculate similarity
          var listSimilarity := CalculateSimilarityBetweenLists(listOne, listTwo);
          WriteLn('Total List Similarity: ' + listSimilarity.ToString);
        end;

        Inc(testNum);
        listOne.Clear;
        listTwo.Clear;
        WriteLn('===============================');
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
