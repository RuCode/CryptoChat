unit Engine.DataBases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, sqlite3conn, FileUtil, Dialogs, md5, SQLite3, SQLite3Wrap;

const
  INVALID_VALUE = -1;

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
  public
    {: Добавление нового пользователя}
    function AddUser(const NickName, Password, Email: string; const AvatarFileName: string = ''): boolean;
    {: Удаление пользователя}
    function RemoveUser(UserID: integer): boolean;
    {: Установить NickName пользователю}
    function SetUserNickName(UserID: integer; const NickName: string): boolean;
    {: Установить Email пользователю}
    function SetUserEMail(UserID: integer; const EMail: string): boolean;
    {: Установить аватарку пользователю из файла}
    function SetUserAvatar(UserID: integer; const AvatarFileName: string = ''): boolean;
    {: Получить NickName пользователя}
    function GetUserNickName(UserID: integer): string;
    {: Получить Email пользователя}
    function GetUserEMail(UserID: integer): string;
    {: Получить PasswordHash пользователя}
    function GetUserPasswordHash(UserID: integer): string;
    {: Сохранить аватарку пользоватля в поток}
    function SaveUserAvatarToStream(UserID: integer; Stream: TStream): boolean;
    {: Получить информацию о пользователе}
    function GetUserInfo(UserID: integer; out NickName, PasswordHash, Email: string): boolean;
    {: Получить количество пользователей в БД}
    function GetUsersCount: integer;
    {: Узнать наличие пользователя в БД}
    function UserExist(AnyStrData: string): integer;
  public
    {: Добавить нового друга}
    function AddFriend(UserID: integer; const NickName, Email: string; const AvatarFileName: string = ''): boolean;
    {: Удалить друга}
    function RemoveFriend(UserID, FriendID: integer): boolean;
    {: Установить новое имя пользователю}
    function SetFriendNickName(UserID, FriendID: integer; NickName: string): boolean;
    {: Присвоить новую почту пользователю}
    function SetFriendEmail(UserID, FriendID: integer; EMail: string): boolean;
    {: Получить имя пользователя}
    function GetFriendNickName(UserID, FriendID: integer): string;
    {: Получить email пользователя}
    function GetFriendEmail(UserID, FriendID: integer): string;
  end;

implementation

{ TCustomDataBase }

procedure TCustomDataBase.UpdateGeneralTableStructure;
// Обновление структур основных таблиц в базе данных
begin
  // Таблица с описанием пользователей программы
  ExecSQL('CREATE TABLE IF NOT EXISTS USERS (''ID'' INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, ''NickName'' TEXT, ''Email'' TEXT, ''PasswordHash'' TEXT, ''AVATAR'' BLOB);');

  // Таблица с описанием друзей
  ExecSQL('CREATE TABLE IF NOT EXISTS FRIENDS (''USER'' INTEGER REFERENCES ID(USERS), ''ID'' INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, ''NickName'' TEXT, ''Email'' TEXT, ''AVATAR'' BLOB);');

  // Таблица с сообщениями
  ExecSQL('CREATE TABLE IF NOT EXISTS MESSAGES (''User'' INTEGER REFERENCES ID(USERS), ''Friend'' INTEGER REFERENCES ID(Friends), ''ID'' INTEGER, ''MessageDate'' DATETIME, '
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

function TCustomDataBase.RemoveUser(UserID: integer): boolean;
  // Удаление пользователя
begin
  Result := ExecSQL(Format('DELETE FROM USERS WHERE ID = %d', [UserID]));
end;

function TCustomDataBase.SetUserNickName(UserID: integer; const NickName: string): boolean;
  // Установить NickName пользователю
var
  Stmt: TSQLite3Statement;
begin
  EnterCriticalsection(CriticalSection);
  try
    Result := True;
    try
      Stmt := SqliteDatabase.Prepare('UPDATE USERS SET NickName = ? WHERE ID = ?');
      Stmt.BindText(1, WideString(NickName));
      Stmt.BindInt(2, UserID);
      Stmt.Step;
      Stmt.Free;
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.SetUserEMail(UserID: integer; const EMail: string): boolean;
  // Установить Email пользователю
var
  Stmt: TSQLite3Statement;
begin
  EnterCriticalsection(CriticalSection);
  try
    Result := True;
    try
      Stmt := SqliteDatabase.Prepare('UPDATE USERS SET EMail = ? WHERE ID = ?');
      Stmt.BindText(1, WideString(EMail));
      Stmt.BindInt(2, UserID);
      Stmt.Step;
      Stmt.Free;
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

function TCustomDataBase.GetUserNickName(UserID: integer): string;
  // Получить NickName пользователя
var
  Stmt: TSQLite3Statement;
begin
  Result := '';
  EnterCriticalsection(CriticalSection);
  try
    Stmt := SqliteDatabase.Prepare('SELECT NickName FROM USERS WHERE ID = ' + WideString(IntToStr(UserID)));
    Stmt.Step;
    Result := string(Stmt.ColumnText(0));
  finally
    Stmt.Free;
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.GetUserEMail(UserID: integer): string;
  // Получить Email пользователя
var
  Stmt: TSQLite3Statement;
begin
  Result := '';
  EnterCriticalsection(CriticalSection);
  try
    Stmt := SqliteDatabase.Prepare('SELECT Email FROM USERS WHERE ID = ' + WideString(IntToStr(UserID)));
    Stmt.Step;
    Result := string(Stmt.ColumnText(0));
  finally
    Stmt.Free;
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.GetUserPasswordHash(UserID: integer): string;
  // Получить PasswordHash пользователя
var
  Stmt: TSQLite3Statement;
begin
  Result := '';
  EnterCriticalsection(CriticalSection);
  try
    Stmt := SqliteDatabase.Prepare('SELECT PasswordHash FROM USERS WHERE ID = ' + WideString(IntToStr(UserID)));
    Stmt.Step;
    Result := string(Stmt.ColumnText(0));
  finally
    Stmt.Free;
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
  EnterCriticalsection(CriticalSection);
  try
    Stmt := SqliteDatabase.Prepare('SELECT AVATAR FROM USERS WHERE ID = ' + WideString(IntToStr(UserID)));
    try
      Stmt.Step;
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
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.GetUserInfo(UserID: integer; out NickName, PasswordHash, Email: string): boolean;
  // Получить информацию о пользователе
var
  Stmt: TSQLite3Statement;
begin
  Result := True;
  EnterCriticalsection(CriticalSection);
  try
    try
      Stmt := SqliteDatabase.Prepare('SELECT NickName, EMail, PasswordHash FROM USERS WHERE ID = ' + WideString(IntToStr(UserID)));
      Stmt.Step;
      NickName := string(Stmt.ColumnText(0));
      Email := string(Stmt.ColumnText(1));
      PasswordHash := string(Stmt.ColumnText(2));
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.GetUsersCount: integer;
  // Получить количество пользователей
var
  Stmt: TSQLite3Statement;
begin
  Result := INVALID_VALUE;
  EnterCriticalsection(CriticalSection);
  try
    Stmt := SqliteDatabase.Prepare('SELECT COUNT(ID) FROM USERS');
    Stmt.Step;
    Result := Stmt.ColumnInt(0);
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.UserExist(AnyStrData: string): integer;
  // Узнать наличие пользователя в БД
var
  Stmt: TSQLite3Statement;
begin
  Result := INVALID_VALUE;
  EnterCriticalsection(CriticalSection);
  try
    Stmt := SqliteDatabase.Prepare('SELECT ID, NickName FROM USERS');
    while Stmt.Step = SQLITE_ROW do
      if Stmt.ColumnText(1) = WideString(AnyStrData) then
        Exit(Stmt.ColumnInt(0));
    Stmt := SqliteDatabase.Prepare('SELECT ID, Email FROM USERS');
    while Stmt.Step = SQLITE_ROW do
      if Stmt.ColumnText(1) = WideString(AnyStrData) then
        Exit(Stmt.ColumnInt(0));
    Stmt := SqliteDatabase.Prepare('SELECT ID, PasswordHash FROM USERS');
    while Stmt.Step = SQLITE_ROW do
      if Stmt.ColumnText(1) = WideString(AnyStrData) then
        Exit(Stmt.ColumnInt(0));
  finally
    Stmt.Free;
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.AddFriend(UserID: integer; const NickName, Email: string; const AvatarFileName: string): boolean;
  // Добавить нового друга
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
        Stmt := SqliteDatabase.Prepare('INSERT INTO FRIENDS (User, NickName, Email, AVATAR) VALUES (?, ?, ?, ?)');
        Stream := TMemoryStream.Create;
        Stream.LoadFromFile(AvatarFileName);
        Stmt.BindInt(1, UserID);
        Stmt.BindText(2, WideString(NickName));
        Stmt.BindText(3, WideString(EMail));
        Stmt.BindBlob(4, Stream.Memory, Stream.Size);
        Stream.Free;
        Stmt.Step;
        Stmt.Free;
      end
      else
      begin
        SqliteDatabase.Execute(WideString(Format('INSERT INTO FRIENDS (User, NickName, Email) VALUES (''%d'', ''%s'', ''%s'');',
          [UserID, NickName, Email])));
      end;
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.RemoveFriend(UserID, FriendID: integer): boolean;
  // Удалить друга
begin
  Result := ExecSQL(Format('DELETE FROM FRIENDS WHERE USER = %d AND ID = %d', [UserID, FriendID]));
end;

function TCustomDataBase.SetFriendNickName(UserID, FriendID: integer; NickName: string): boolean;
  // Установить новое имя пользователю
var
  Stmt: TSQLite3Statement;
begin
  EnterCriticalsection(CriticalSection);
  try
    Result := True;
    try
      Stmt := SqliteDatabase.Prepare('UPDATE FRIENDS SET NickName = ? WHERE USER = ? AND ID = ?');
      Stmt.BindText(1, WideString(NickName));
      Stmt.BindInt(2, UserID);
      Stmt.BindInt(3, FriendID);
      Stmt.Step;
      Stmt.Free;
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.SetFriendEmail(UserID, FriendID: integer; EMail: string): boolean;
  // Присвоить новую почту пользователю
var
  Stmt: TSQLite3Statement;
begin
  EnterCriticalsection(CriticalSection);
  try
    Result := True;
    try
      Stmt := SqliteDatabase.Prepare('UPDATE FRIENDS SET EMail = ? WHERE USER = ? AND ID = ?');
      Stmt.BindText(1, WideString(EMail));
      Stmt.BindInt(2, UserID);
      Stmt.BindInt(3, FriendID);
      Stmt.Step;
      Stmt.Free;
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.GetFriendNickName(UserID, FriendID: integer): string;
  // Получить имя пользователя
var
  Stmt: TSQLite3Statement;
begin
  Result := '';
  EnterCriticalsection(CriticalSection);
  try
    Stmt := SqliteDatabase.Prepare(WideString(Format('SELECT NickName FROM FRIENDS WHERE USER = %d AND ID = %d', [UserID, FriendID])));
    Stmt.Step;
    Result := string(Stmt.ColumnText(0));
  finally
    Stmt.Free;
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.GetFriendEmail(UserID, FriendID: integer): string;
  // Получить email пользователя
var
  Stmt: TSQLite3Statement;
begin
  Result := '';
  EnterCriticalsection(CriticalSection);
  try
    Stmt := SqliteDatabase.Prepare(WideString(Format('SELECT EMail FROM FRIENDS WHERE USER = %d AND ID = %d', [UserID, FriendID])));
    Stmt.Step;
    Result := string(Stmt.ColumnText(0));
  finally
    Stmt.Free;
    LeaveCriticalsection(CriticalSection);
  end;
end;

initialization
  {$IFDEF WINDOWS}// Windows
  SQLiteLibraryName := 'sqlite3.dll';
  {$ENDIF}

end.
