unit Engine.DataBases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sqlite3DS, sqldb, sqlite3conn, FileUtil, Dialogs, md5;

const
  // Названия столбцов таблиц
  SQL_COL_ID_USER = 'ID_USER';
  SQL_COL_ID_FRIEND = 'ID_FRIEND';
  SQL_COL_AUTH = 'AUTH';
  SQL_COL_UUID = 'UUID';
  SQL_COL_NICK_NAME = 'NNAME';
  SQL_COL_EMAIL = 'EMAIL';
  SQL_COL_AVATAR = 'ADATA';
  SQL_COL_RESP_DATE = 'RDATE';
  SQL_COL_XID = 'XID';
  SQL_COL_IS_MYMSG = 'ISMY';
  SQL_COL_OPEN_KEY = 'OPEN';
  SQL_COL_SECRET_KEY = 'SECRET';
  SQL_COL_BF_KEY = 'BF';
  SQL_COL_ZIP_DATA = 'ZDATA';
  SQL_COL_HASH = 'HASH';
  SQL_COL_MESSAGE = 'MESSAGE';

  // Таблицы
  SQL_TBL_USERS = 'USERS';
  SQL_TBL_FRIENDS = 'FRIENDS';
  SQL_TBL_MESSAGES = 'MESSAGES';

type

  { TCustomDataBase }

  TCustomDataBase = class(TObject)
  private
    // Для основной работы
    CriticalSection: TRTLCriticalSection;
    Sqlite3Dataset: TSqlite3Dataset;
    // Для создания бд
    SQLite3Connection: TSQLite3Connection;
    SQLTransaction: TSQLTransaction;
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
    function AddUser(NickName, Password, Email: string; AvatarFileName: string = ''): Boolean;
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
  Sqlite3Dataset := TSqlite3Dataset.Create(nil);
  SQLite3Connection := TSQLite3Connection.Create(nil);
  SQLTransaction := TSQLTransaction.Create(nil);
  SQLite3Connection.Transaction := SQLTransaction;
  SQLTransaction.DataBase := SQLite3Connection;
end;

destructor TCustomDataBase.Destroy;
begin
  SQLTransaction.Free;
  SQLite3Connection.Free;
  Sqlite3Dataset.Free;
  inherited Destroy;
end;

function TCustomDataBase.CreateDataBase(FileName: string): boolean;
  // Создание новой базы данных
begin
  try
    EnterCriticalsection(CriticalSection);
    SQLite3Connection.Close;
    try
      SQLite3Connection.DatabaseName := FileName;
      if not FileExists(SQLite3Connection.DatabaseName) then
      begin
        try
          // Создаём пустую баз данных
          SQLite3Connection.Open;
          SQLTransaction.Active := True;
          SQLite3Connection.ExecuteDirect('CREATE TABLE MAIN (Code integer NOT NULL);');
          SQLTransaction.Commit;
          SQLite3Connection.Close;
          Result := OpenDataBase(FileName);
        except
          raise Exception.Create('Не могу создать новую базу данных...');
        end;
      end;
    except
      raise Exception.Create('Не могу проверить наличие базы данных...');
    end;
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
      Sqlite3Dataset.Close;
      Sqlite3Dataset.FileName := DBPath;
      Sqlite3Dataset.TableName := 'MAIN';
      Sqlite3Dataset.Open;
      Result := Sqlite3Dataset.ReturnCode = 101 {NOT A ERROR};
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
      Sqlite3Dataset.Close;
      Sqlite3Dataset.SQL := SQLText;
      Sqlite3Dataset.ExecSQL;
      Result := Sqlite3Dataset.ReturnCode = 101 {NOT A ERROR};
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

function TCustomDataBase.AddUser(NickName, Password, Email: string;
  AvatarFileName: string): Boolean;
// Создание нового пользователя
begin
  EnterCriticalsection(CriticalSection);
  try
    try
      Sqlite3Dataset.Close;
      if AvatarFileName <> '' then
      begin
        Sqlite3Dataset.SQL := Format('INERT INTO USERS (NickName, Email, PasswordHash, AVATAR) VALUES (%s, %s, %s, :Avatar);',
          [NickName, Email, MD5Print(MD5String(Password))]);
        //        FieldByName('Hash').bloAsString := MD5Print(MD5String(Password));
        // Не ясно как добавить
      end
      else
        Sqlite3Dataset.SQL := Format('INSERT INTO USERS (NickName, Email, PasswordHash) VALUES (''%s'', ''%s'', ''%s'');',
          [NickName, Email, MD5Print(MD5String(Password))]);

      Sqlite3Dataset.ExecSQL;

      Result := Sqlite3Dataset.ReturnCode = 101 {NOT A ERROR};
    except
      Result := False;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

initialization
  {$IFDEF WINDOWS}// Windows
  SQLiteLibraryName := 'sqlite3.dll';
  {$ENDIF}

end.
