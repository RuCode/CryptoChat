unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Engine.Mail;

type

  { TMainForm }

  TMainForm = class(TForm)
    EditPortOut: TLabeledEdit;
    EditUserName: TLabeledEdit;
    EditUserPassword: TLabeledEdit;
    EditHostIn: TLabeledEdit;
    EditPortIn: TLabeledEdit;
    MainMenu: TMainMenu;
    Memo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
  private
    procedure SetLogLine(AValue: string);
    { private declarations }
  public
    { public declarations }
    mail: TCustomMail;
    procedure BeginNewFunc;
    property Log: string write SetLogLine;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
// Создание
begin
  mail := TCustomMail.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// Уничтожение
begin
  mail.Free;
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
// Получить список папок
var
  list: TStringList;
  str: string;
begin
  BeginNewFunc;
  list := TStringList.Create;
  mail.GetFolderList(list);
  for str in list do
    Memo.Lines.Add(str);
  list.Free;
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
// Выбрать папку
begin
  BeginNewFunc;
  mail.SelectedFolder := 'INBOX';
  Log := mail.FullResult.Text;
end;

procedure TMainForm.MenuItem4Click(Sender: TObject);
// Подключиться
begin
  BeginNewFunc;
  mail.Connected := True;
  Log := BoolToStr(mail.Connected, 'Connected', 'Error');
  Log := mail.FullResult.Text;
end;

procedure TMainForm.MenuItem5Click(Sender: TObject);
// Узнать количество новых писем
begin
  BeginNewFunc;
  Log := IntToStr(mail.CountOfNewMails);
end;

procedure TMainForm.SetLogLine(AValue: string);
// Занести в лог
begin
  Memo.Append(AValue);
end;

procedure TMainForm.BeginNewFunc;
// Разделитель для красоты
var
  i: integer;
  buf: string;
begin
  buf := '';
  for i := 0 to 40 do
    buf := buf + '#';
  log := buf;
end;

end.

