unit Engine.Tar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LibTar;

const
  ERRORTAR_NOT_OPEN_FILE = 'Для выполнения текущей операции следует предварительно открыть файл *.TAR.';

type
  TActionForTar = (aftClosed, aftRead, aftWrite);


  { TCustomTar }

  TCustomTar = class
  private
    fTarWriter: TTarWriter;
    fTarReader: TTarArchive;
    fTarRec: TTarDirRec;
    fCurrentAction: TActionForTar;
  public
    constructor Create;
    procedure OpenTar(FileName: string; Action: TActionForTar = aftRead);
    procedure AddTextFile(FileName: string);
    procedure AddBinaryFile(FileName: string);
    procedure ExtractFile(TarFileName, FileName: string);
  end;

implementation

{ TCustomTar }

constructor TCustomTar.Create;
begin
  fCurrentAction := aftClosed;
end;

procedure TCustomTar.OpenTar(FileName: string; Action: TActionForTar);
begin
  if Action = aftWrite then
    fTarWriter := TTarWriter.Create(FileName)
  else
    aftRead:
      fTarReader := TTarArchive.Create(FileName, fmOpenRead);
  fCurrentAction := Action;

{  var
    TarWriter: TTarWriter;
    TarReader: TTarArchive;
    TarRec: TTarDirRec;
    szBuf: String;
  begin
    // Write
    WriteLn(#13+#10+'Create TAR');
    Write('Input text: ');
    ReadLn(szBuf);
    TarWriter:= TTarWriter.Create('new.tar');
    TarWriter.AddString(szBuf, 'main.txt', now);
    TarWriter.Free;
    WriteLn('File main.txt added to TAR');
    // Read
    WriteLn(#13+#10+'Read main.txt from TAR');
    TarReader:= TTarArchive.Create('new.tar', fmOpenRead);
    TarReader.ReadFile;
    TarRec.Name:= 'main.txt';
    TarReader.FindNext(TarRec);
    WriteLn(TarReader.ReadFile);
    TarReader.Free;
    ReadLn;

 }
end;

procedure TCustomTar.AddTextFile(FileName: string);
// Добавляем текстовый файл в архив
var
  StringList: TStringList;
begin
  if fCurrentAction = aftClosed then
    raise Exception.Create(ERRORTAR_NOT_OPEN_FILE);
  StringList := TStringList.Create;
  StringList.LoadFromFile(FileName);
  fTarWriter.AddString(StringList.Text, ExtractFileName(FileName), now);
  StringList.Free;
end;

procedure TCustomTar.AddBinaryFile(FileName: string);
// Добавляет бинарный файл в архив
begin
  if fCurrentAction = aftClosed then
    raise Exception.Create(ERRORTAR_NOT_OPEN_FILE);
  fTarWriter.AddFile(FileName, ExtractFileName(FileName));
end;

procedure TCustomTar.ExtractFile(TarFileName, FileName: string);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  fTarReader.Reset;
  TarRec.Name := TarFileName;
  TarReader.FindNext(TarRec);
  StringList.Add(TarReader.ReadFile);
  StringList.SaveToFile(FileName);
  StringList.Free;
end;

end.
