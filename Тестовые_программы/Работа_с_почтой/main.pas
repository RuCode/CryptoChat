unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Engine.Mail;

type

  { TMainForm }

  TMainForm = class(TForm)
    EditUserName: TLabeledEdit;
    EditUserPassword: TLabeledEdit;
    EditHost: TLabeledEdit;
    EditPortIn: TLabeledEdit;
    MainMenu: TMainMenu;
    Memo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
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

procedure TMainForm.MenuItem10Click(Sender: TObject);
// Получить имя вложения
var
  StringList: TStringList;
begin
  BeginNewFunc;
  StringList := TStringList.Create;
  StringList.Text := Mail.GetMailBody(StrToInt(InputBox('Ввод', 'Введите индекс письма:', '')));
  Log := UTF8ToSys('Имя первого вложения: ') + Mail.GetMailAttachFileName(StringList, 1);
  StringList.Free;
end;

procedure TMainForm.MenuItem12Click(Sender: TObject);
// Сохранить вложение
begin
  BeginNewFunc;
  Mail.SaveAttachToFile(1, 1, ExtractFilePath(Application.ExeName) + 'attach.pdf');
  Log := 'Сохранено в attach.pdf';
end;

procedure TMainForm.MenuItem13Click(Sender: TObject);
// Получить информацию о письме
begin
  Log := UTF8ToSys('От: ') + Mail.GetEmailFrom(1);
  Log := UTF8ToSys('К: ') + Mail.GetEmailTo(1);
  Log := UTF8ToSys('Тема: ') + Mail.GetEmailSubject(1);
end;

procedure TMainForm.MenuItem14Click(Sender: TObject);
// Отправка письма
begin
  BeginNewFunc;
  Log := BoolToStr(Mail.SendMail(EditUserName.Text, 'antony@email.su', 'Тема', 'Текст', ''), 'Отправлено', 'Ошибка');
  Log := mail.FullResult.Text;
  Log := mail.ResultString;
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
  mail.UserName := EditUserName.Text;
  mail.Password := EditUserPassword.Text;
  mail.IMAPHost := EditHost.Text;
  mail.IMAPPort := StrToInt(EditPortIn.Text);
  mail.Connected := True;
  Log := BoolToStr(mail.Connected, 'Connected', 'Error');
  Log := mail.FullResult.Text;
end;

procedure TMainForm.MenuItem5Click(Sender: TObject);
// Узнать количество новых писем
begin
  BeginNewFunc;
  Log := UTF8ToSys('Новых писем: ') + IntToStr(mail.CountOfNewMails);
  Log := UTF8ToSys('Всего писем: ') + IntToStr(mail.CountOfMails);
end;

procedure TMainForm.MenuItem6Click(Sender: TObject);
// Получить заголовок сообщения
begin
  BeginNewFunc;
  Log := Mail.GetMailHeader(StrToInt(InputBox('Ввод', 'Введите индекс письма:', '')));
end;

procedure TMainForm.MenuItem7Click(Sender: TObject);
// Получить сообщение
begin
  BeginNewFunc;
  Log := Mail.GetMailBody(StrToInt(InputBox('Ввод', 'Введите индекс письма:', '')));
end;

procedure TMainForm.MenuItem8Click(Sender: TObject);
// Получить текст письма
var
  StringList: TStringList;
begin
  BeginNewFunc;
  StringList := TStringList.Create;
  StringList.Text := Mail.GetMailBody(StrToInt(InputBox('Ввод', 'Введите индекс письма:', '')));
  Log := Mail.GetMailText(StringList);
  StringList.Free;
end;

procedure TMainForm.MenuItem9Click(Sender: TObject);
// Получить количество вложений
var
  StringList: TStringList;
begin
  BeginNewFunc;
  StringList := TStringList.Create;
  StringList.Text := Mail.GetMailBody(StrToInt(InputBox('Ввод', 'Введите индекс письма:', '')));
  Log := UTF8ToSys('Вложений: ') + IntToStr(Mail.GetMailAttachCount(StringList));
  StringList.Free;
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

