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
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
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
  Memo.Lines.Add(BoolToStr(DB.AddUser('Test1', 'pass1', 'Test1@Email.ru'), 'Новый пользователь создан', 'ошибка'));
  if OpenDialog.Execute then
    Memo.Lines.Add(BoolToStr(DB.AddUser('Test2', 'pass2', 'Test2@Email.ru', OpenDialog.FileName), 'Новый пользователь создан', 'ошибка'));
end;

procedure TMainForm.MenuItem7Click(Sender: TObject);
// Установить новую аватарку
begin
  if OpenDialog.Execute then
    Memo.Lines.Add(BoolToStr(DB.SetUserAvatar(1, OpenDialog.FileName), 'Установлена', 'Не установлена'));
end;

procedure TMainForm.MenuItem8Click(Sender: TObject);
// Сохранить аватарку пользователя
var
  MemoryStream: TMemoryStream;
begin
  if SaveDialog.Execute then
  begin
    MemoryStream := TMemoryStream.Create;
    DB.SaveUserAvatarToStream(2, MemoryStream);
    MemoryStream.SaveToFile(SaveDialog.FileName);
    MemoryStream.Free;
  end;
end;

procedure TMainForm.MenuItem9Click(Sender: TObject);
var
  EMail: string;
  PassHash: string;
  Nick: string;
begin
  DB.GetUserInfo(1, Nick, PassHash, EMail);
  Memo.Lines.Add('NickName:   ' + Nick);
  Memo.Lines.Add('PassHash:   ' + PassHash);
  Memo.Lines.Add('EMail:      ' + EMail);
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
  DeleteFile(DBPath);
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
