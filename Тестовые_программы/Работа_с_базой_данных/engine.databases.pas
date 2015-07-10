unit Engine.DataBases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sqlite3DS, sqldb, sqlite3conn, FileUtil, Dialogs;

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
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {: Создание новой базы данных}
    function CreateDataBase(FileName: string): boolean;
    {: Открытие БД SQLite}
    function OpenDataBase(DBPath: string): boolean;
    {: Выполнить произвольный запрос}
    function ExecSQL(SQLText: string): boolean;
  end;

implementation

{ TCustomDataBase }

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
          SQLite3Connection.Open;
          SQLTransaction.Active := True;
          SQLite3Connection.ExecuteDirect('CREATE TABLE MAIN (Code integer NOT NULL);');
          SQLTransaction.Commit;
          SQLite3Connection.Close;
          OpenDataBase(FileName);
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

end.

