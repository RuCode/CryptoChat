unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sqlite3DS, sqldb, sqlite3conn, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Menus, engine.databases;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    Memo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    SQLite3Connection1: TSQLite3Connection;
    Sqlite3Dataset: TSqlite3Dataset;
    SQLTransaction1: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    db: TCustomDataBase;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.MenuItem4Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
var
  DBPath: string;
begin
  DBPath := ExtractFilePath(Application.ExeName) + DirectorySeparator + 'database2.sqlite3';
  Memo.Lines.Add(BoolToStr(DB.OpenDataBase(DBPath), 'Смог открыть', 'Не смог открыть'));
  //  Memo.Lines.Add(BoolToStr(DB.ExecSQL('CREATE TABLE USERS (Code integer NOT NULL)'), 'Смог выполнить запрос', 'Не смог выполнить запрос'));
  DBPath := ExtractFilePath(Application.ExeName) + DirectorySeparator + 'database.sqlite3';
  Memo.Lines.Add(BoolToStr(DB.CreateDataBase(DBPath), 'Смог выполнить запрос', 'Не смог выполнить запрос'));
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  db := TCustomDataBase.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DB.Free;
end;

end.


