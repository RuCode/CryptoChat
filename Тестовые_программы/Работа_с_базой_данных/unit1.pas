unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, engine.databases;

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
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
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

procedure TMainForm.MenuItem6Click(Sender: TObject);
// Создание нового пользователя
begin
  Memo.Lines.Add(BoolToStr(DB.AddUser('Test', 'pass', 'Test@Email.ru'), 'Новый пользователь создан', 'ошибка'));
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
// Открытие и создание БД
var
  DBPath: string;
begin
  Memo.Lines.Add('ПРОХОДИМ ТЕСТИРОВАНИЕ ОТКРЫТИЯ И СОЗДАНИЯ БД....');
  DBPath := ExtractFilePath(Application.ExeName) + DirectorySeparator + 'database2.sqlite3';
  Memo.Lines.Add(BoolToStr(DB.OpenDataBase(DBPath), 'Смог открыть', 'Не смог открыть'));
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


