unit Utils;

interface

uses Generics.Collections;

type
  TFileBlock = class
    IsFile: Boolean;
    FileID: Integer;
    StartIdx: Integer;
    Length: Integer;
    Checked: Boolean;
  end;

procedure GenerateIndexes(aMap: string; aFileBlocks: TList<TFileBlock>);
function CalculateChecksum(aFileBlocks: TList<TFileBlock>): Int64;
function ManipulateFileBlocks(var aFileBlocks: TList<TFileBlock>): Int64;

implementation

uses System.SysUtils, StrUtils, Generics.Defaults;

procedure GenerateIndexes(aMap: string; aFileBlocks: TList<TFileBlock>);
begin
  var fileID := 0;
  for var i := 1 to Length(aMap) do
  begin
    var fileBlock := TFileBlock.Create;
    fileBlock.Length := StrToInt(aMap[i]);
    fileBlock.IsFile := (i mod 2) <> 0;
    fileBlock.Checked := False;
    if aFileBlocks.Count = 0 then
      fileBlock.StartIdx := 0
    else
    fileBlock.StartIdx := aFileBlocks[aFileBlocks.Count-1].StartIdx + aFileBlocks[aFileBlocks.Count-1].Length;

    if fileBlock.IsFile then
    begin
      fileBlock.FileID := fileID;
      Inc(fileID);
    end
    else
    begin
      fileBlock.FileID := -1;
    end;
    aFileBlocks.Add(fileBlock);
  end;
end;

function CalculateChecksum(aFileBlocks: TList<TFileBlock>): Int64;
begin
  var diskSpread := TList<Integer>.Create;
  var emptyIndexes := TList<Integer>.Create;
  var filledIndexes := TList<Integer>.Create;

  try
    for var fileBlock in aFileBlocks do
    begin
      if fileBlock.IsFile then
      begin
        for var j := 0 to fileBlock.Length - 1 do
        begin
          diskSpread.Add(fileBlock.FileID);
          filledIndexes.Add(diskSpread.Count - 1);
        end;
      end
      else
      begin
        for var j := 0 to fileBlock.Length - 1 do
        begin
          diskSpread.Add(-1);
          emptyIndexes.Add(diskSpread.Count - 1);
        end;
      end;
    end;

    Result := 0;
    var emptyIdx := 0;

    for var i := filledIndexes.Count - 1 downto 0 do
    begin
      var fileIdx := filledIndexes[i];

      if diskSpread[fileIdx] = -1 then
        WriteLn('Error');

      if fileIdx < filledIndexes.Count then
        Result := Result + (fileIdx * diskSpread[fileIdx])
      else
      begin
        Result := Result + (emptyIndexes[emptyIdx] * diskSpread[fileIdx]);
        Inc(emptyIdx);
      end;
    end;
  finally
    diskSpread.Free;
    emptyIndexes.Free;
    filledIndexes.Free;
  end;
end;

function ManipulateFileBlocks(var aFileBlocks: TList<TFileBlock>): Int64;
begin
  for var i := aFileBlocks.Count - 1 downto 0 do
  begin
    if aFileBlocks[i].IsFile then
    begin
      for var j := 0 to i do
      begin
        if not aFileBlocks[j].IsFile then
        begin
          if aFileBlocks[j].Length >= aFileBlocks[i].Length then
          begin
            //Create new fileblock for now occupied space
            var newFile := TFileBlock.Create;
            newFile.StartIdx := aFileBlocks[j].StartIdx;
            newFile.Length := aFileBlocks[i].Length;
            newFile.FileID := aFileBlocks[i].FileID;
            newFile.IsFile := aFileBlocks[i].IsFile;

            //adjust empty block
            aFileBlocks[j].Length := aFileBlocks[j].Length - newFile.Length;
            aFileBlocks[j].StartIdx := newFile.StartIdx + newFile.Length;

            //Convert original block to empty space
            aFileBlocks[i].IsFile := False;
            aFileBlocks[i].FileID := -1;

            aFileBlocks.Insert(j, newFile);
            Break;
          end;
        end;
      end;
    end;
  end;

  aFileBlocks.Sort(TComparer<TFileBlock>.Construct(
  function(const a,b:TFileBlock):Integer
  begin
    Result := a.StartIdx - b.StartIdx;
  end));

  Result := 0;
  for var block in aFileBlocks do
  begin
    if block.IsFile then
      for var i := 0 to block.Length - 1 do
        Result := Result + (block.FileID * (block.StartIdx + i));
  end;
end;


end.
