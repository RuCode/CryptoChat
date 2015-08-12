{
  Задача, создание архива в памяти и преобразование в форму для отправки письма.
  Скорее всего все операции будут делаться в памяти, так что операции будут ограничены потоками...

  Функционал для создания письма:
    - Создать поток архива
    - Добавить в поток файл

  Функционал для чтения письма:
    - Открыть поток с ахривом
    - Выделить файл из потока-архива в отдельный поток
}

unit Tars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LibTar;

const
  ERRORTAR_NOT_OPEN_FILE =
    'Для выполнения текущей операции следует предварительно открыть файл *.TAR.';

type
  TActionForTar = (aftClosed, aftRead, aftWrite);


  { TCustomTar }

  TCustomTar = class
  private
    {: Проекция архива в памяти}
    fArchiveStream: TMemoryStream;
    {: Для записи в архива}
    fTarWriter: TTarWriter;
    {: Для чтение из архива}
    fTarReader: TTarArchive;
    {: Для поиска в архиве}
    fTarRec: TTarDirRec;
    {: Для восстановления позиции в потоке архива}
    fLastPos: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    {: Добавление нового файла из потока в поток-архив}
    function AddFile(TarFileName: string; Stream: TStream): boolean; overload;
    {: Добавление нового файла с диска в поток-архив}
    function AddFile(TarFileName: string; FilePath: string): boolean; overload;
    {: Добавление текстового файла}
    function AddTextFile(TarFileName: string; BodyText: string): boolean;
    {: Извлечение файла из потока}
    function ExtractFile(TarFileName: string; var Stream: TStream): boolean;
    {: Извлечение файла из потока}
    function ExtractFile(TarFileName: string; FilePath: string): boolean;

    {: Сохранение архива в файл}
    procedure StoreToFile(FilePath: string);
    {: Сохранение архива в поток}
    procedure StoreToStream(var Stream: TStream);
    {: Загрузка архива в файл}
    procedure LoadFromFile(FilePath: string);
    {: Загрузка архива в поток}
    procedure LoadFromStream(var Stream: TStream);

  end;

  TTar = TCustomTar;

implementation

{ TCustomTar }

constructor TCustomTar.Create;
begin
  fArchiveStream := TMemoryStream.Create;
  fLastPos := 0;
end;

destructor TCustomTar.Destroy;
begin
  fArchiveStream.Free;
end;

function TCustomTar.AddFile(TarFileName: string; Stream: TStream): boolean;
  // Добавление файла в поток-архива
begin
  Result := True;
  try
    try
      fArchiveStream.Position := fLastPos;
      fTarWriter := TTarWriter.Create(fArchiveStream);
      fTarWriter.AddStream(Stream, TarFileName, Now);
      fLastPos := fArchiveStream.Position;
    except
      Result := False;
    end;
  finally
    fTarWriter.Free;
  end;
end;

function TCustomTar.AddFile(TarFileName: string; FilePath: string): boolean;
  // Добавление файла в поток-архива
begin
  Result := True;
  try
    try
      fArchiveStream.Position := fLastPos;
      fTarWriter := TTarWriter.Create(fArchiveStream);
      fTarWriter.AddFile(FilePath, TarFileName);
      fLastPos := fArchiveStream.Position;
    except
      Result := False;
    end;
  finally
    fTarWriter.Free;
  end;
end;

function TCustomTar.AddTextFile(TarFileName: string; BodyText: string): boolean;
// Добавление текстового файла
begin
  Result := True;
  try
    try
      fArchiveStream.Position := fLastPos;
      fTarWriter := TTarWriter.Create(fArchiveStream);
      fTarWriter.AddString(BodyText, TarFileName, Now);
      fLastPos := fArchiveStream.Position;
    except
      Result := False;
    end;
  finally
    fTarWriter.Free;
  end;
end;

function TCustomTar.ExtractFile(TarFileName: string; var Stream: TStream): boolean;
  // Извлечение файла
begin
  Result := True;
  try
    try
      fTarReader := TTarArchive.Create(fArchiveStream);
      fTarReader.Reset;
      fTarRec.Name := TarFileName;
      fTarReader.FindNext(fTarRec);
      fTarReader.ReadFile(Stream);
    except
      Result := False;
    end;
  finally
    fTarReader.Free;
  end;
end;

function TCustomTar.ExtractFile(TarFileName: string; FilePath: string): boolean;
  // Извлечение файла
begin
  Result := True;
  try
    try
      fTarReader := TTarArchive.Create(fArchiveStream);
      fTarReader.Reset;
      fTarRec.Name := TarFileName;
      fTarReader.FindNext(fTarRec);
      fTarReader.ReadFile(FilePath);
    except
      Result := False;
    end;
  finally
    fTarReader.Free;
  end;
end;

procedure TCustomTar.StoreToFile(FilePath: string);
// Сохранение в файл
begin
  fArchiveStream.SaveToFile(FilePath);
end;

procedure TCustomTar.StoreToStream(var Stream: TStream);
// Сохранение в поток
begin
  fArchiveStream.SaveToStream(Stream);
end;

procedure TCustomTar.LoadFromFile(FilePath: string);
// Загрузка из файла
begin
  fArchiveStream.LoadFromFile(FilePath);
end;

procedure TCustomTar.LoadFromStream(var Stream: TStream);
// Загрузка из потока
begin
  fArchiveStream.LoadFromStream(Stream);
end;

end.
