unit Engine.DataBases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, sqlite3conn, FileUtil, Dialogs, md5, SQLite3Wrap;

type

  { TCustomDataBase }

  TCustomDataBase = class(TObject)
  private
    // Для основной работы
    CriticalSection: TRTLCriticalSection;
    SqliteDatabase: TSQLite3Database;
    {: Обновление структур основных таблиц в базе данных}
    procedure UpdateGeneralTableStructure;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {: Создание новой базы данных}
    function CreateDataBase(FileName: string): boolean;
    {: Открытие БД SQLite}
    function OpenDataBase(DBPath: string): boolean;
    {: Выполнить произвольный запрос}
    function ExecSQL(SQLText: string): boolean;
    {: Добавление нового пользователя}
    function AddUser(const NickName, Password, Email: string; const AvatarFileName: string = ''): boolean;
    {: Изменение аватарки пользователя}
    function SetUserAvatar(UserID: integer; const AvatarFileName: string = ''): boolean;
    {: Сохранить аватарку пользоватля}
    function SaveUserAvatarToStream(UserID: integer; Stream: TStream): boolean;
    {: Получить информацию о пользователе}
    function GetUserInfo(UserID: integer; out NickName, PasswordHash, Email: string): boolean;
  end;

implementation

{ TCustomDataBase }

procedure TCustomDataBase.UpdateGeneralTableStructure;
// Обновление структур основных таблиц в базе данных
begin
  // Таблица с описанием пользователей программы
  SqliteDatabase.Execute(
    'CREATE TABLE IF NOT EXISTS USERS (''ID'' INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, ''NickName'' TEXT, ''Email'' TEXT, ''PasswordHash'' TEXT, ''AVATAR'' BLOB);');

  // Таблица с описанием друзей
  SqliteDatabase.Execute(
    'CREATE TABLE IF NOT EXISTS FRIENDS (''USER'' INTEGER REFERENCES ID(USERS), ''ID'' INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, ''NickName'' TEXT, ''Email'' TEXT, ''AVATAR'' BLOB);');

  // Таблица с сообщениями
  SqliteDatabase.Execute(
    'CREATE TABLE IF NOT EXISTS MESSAGES (''User'' INTEGER REFERENCES ID(USERS), ''Friend'' INTEGER REFERENCES ID(Friends), ''ID'' INTEGER, ''MessageDate'' DATETIME, '
    + '''Self'' BOOLEAN, ''OpenKey'' BLOB, ''PrivateKey'' BLOB, ''BlowFishKey'' BLOB, ''MessageTar'' BLOB);');
end;

constructor TCustomDataBase.Create;
begin
  inherited Create;
end;

destructor TCustomDataBase.Destroy;
begin
  inherited Destroy;
end;

function TCustomDataBase.CreateDataBase(FileName: string): boolean;
  // Создание новой базы данных
begin
  EnterCriticalsection(CriticalSection);
  try
    if Assigned(SqliteDatabase) then
      FreeAndNil(SqliteDatabase);
    SqliteDatabase := TSQLite3Database.Create;
    SqliteDatabase.Open(WideString(FileName));
    UpdateGeneralTableStructure;
    Result := FileExists(FileName);
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.OpenDataBase(DBPath: string): boolean;
  // Открытие БД SQLite
begin
  EnterCriticalsection(CriticalSection);
  try
    try
      Result := CreateDataBase(DBPath);
      if Result then
        UpdateGeneralTableStructure;
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.ExecSQL(SQLText: string): boolean;
  // Выполнить произвольный запрос
begin
  EnterCriticalsection(CriticalSection);
  try
    try
      Result := True;
      SqliteDatabase.Execute(WideString(SQLText));
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.AddUser(const NickName, Password, Email: string; const AvatarFileName: string): boolean;
  // Создание нового пользователя
var
  Stmt: TSQLite3Statement;
  Stream: TMemoryStream;
begin
  EnterCriticalsection(CriticalSection);
  try
    Result := True;
    try
      if AvatarFileName <> '' then
      begin
        Stmt := SqliteDatabase.Prepare('INSERT INTO USERS (NickName, Email, PasswordHash, AVATAR) VALUES (?, ?, ?, ?)');
        Stream := TMemoryStream.Create;
        Stream.LoadFromFile(AvatarFileName);
        Stmt.BindText(1, WideString(NickName));
        Stmt.BindText(2, WideString(EMail));
        Stmt.BindText(3, WideString(MD5Print(MD5String(Password))));
        Stmt.BindBlob(4, Stream.Memory, Stream.Size);
        Stream.Free;
        Stmt.Step;
        Stmt.Free;
      end
      else
      begin
        SqliteDatabase.Execute(WideString(Format('INSERT INTO USERS (NickName, Email, PasswordHash) VALUES (''%s'', ''%s'', ''%s'');',
          [NickName, Email, MD5Print(MD5String(Password))])));
      end;
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.SetUserAvatar(UserID: integer; const AvatarFileName: string): boolean;
  // Установка новой аватарки пользователю
var
  Stmt: TSQLite3Statement;
  Stream: TMemoryStream;
begin
  EnterCriticalsection(CriticalSection);
  try
    Result := True;
    try
      Stmt := SqliteDatabase.Prepare('UPDATE USERS SET AVATAR = ? WHERE ID = ?');
      Stream := TMemoryStream.Create;
      Stream.LoadFromFile(AvatarFileName);
      Stmt.BindBlob(1, Stream.Memory, Stream.Size);
      Stmt.BindInt(2, UserID);
      Stream.Free;
      Stmt.Step;
      Stmt.Free;
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.SaveUserAvatarToStream(UserID: integer; Stream: TStream): boolean;
  // Сохранение пользовательской аватарки в файл
var
  Stmt: TSQLite3Statement;
  MemoryStream: TMemoryStream;
  Size: integer;
begin
  Result := True;
  Stmt := SqliteDatabase.Prepare('SELECT AVATAR FROM USERS WHERE ID = ' + WideString(IntToStr(UserID)));
  Stmt.Step;
  try
    try
        Size := Stmt.ColumnBytes(0);
        MemoryStream := TMemoryStream.Create;
        MemoryStream.SetSize(Size);
        MemoryStream.Write(Stmt.ColumnBlob(0)^, Size);
        MemoryStream.Position := 0;
        MemoryStream.SaveToStream(Stream);
        MemoryStream.Free;
    except
      Result := False;
    end;
  finally
    Stmt.Free;
  end;
end;

function TCustomDataBase.GetUserInfo(UserID: integer; out NickName, PasswordHash, Email: string): boolean;
// Получить информацию о пользователе
var
  Stmt: TSQLite3Statement;
begin
  Result := True;
  try
    Stmt := SqliteDatabase.Prepare('SELECT NickName, EMail, PasswordHash FROM USERS WHERE ID = ' + WideString(IntToStr(UserID)));
    Stmt.Step;
    NickName := string(Stmt.ColumnText(0));
    Email := string(Stmt.ColumnText(1));
    PasswordHash := string(Stmt.ColumnText(2));
  except
    Result := False;
  end;
end;

initialization
  {$IFDEF WINDOWS}// Windows
  SQLiteLibraryName := 'sqlite3.dll';
  {$ENDIF}

end.
