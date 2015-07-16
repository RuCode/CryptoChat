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
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
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
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
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
// Получаем информацию о пользователе
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

procedure TMainForm.MenuItem10Click(Sender: TObject);
// Количество пользователей
begin
  Memo.Lines.Add('Пользователей в базе: ' + IntToStr(DB.GetUsersCount));
end;

procedure TMainForm.MenuItem11Click(Sender: TObject);
// Удаляем друга
begin
  Memo.Lines.Add(BoolToStr(DB.RemoveUser(1), 'Удалён', 'Не удалён'));
end;

procedure TMainForm.MenuItem13Click(Sender: TObject);
// Изменяем данные пользователя
begin
  Memo.Lines.Add(BoolToStr(DB.SetUserEMail(2, 'NewEmail@NewEmail.NewEmail') and DB.SetUserNickName(2, 'NewNickName'), 'Изменено', 'Не изменено'));

  Memo.Lines.Add(DB.GetUserNickName(2));
  Memo.Lines.Add(DB.GetUserEMail(2));
  Memo.Lines.Add(DB.GetUserPasswordHash(2));
end;

procedure TMainForm.MenuItem14Click(Sender: TObject);
// Узнать наличие пользователя
begin
  Memo.Lines.Add('ID: ' + IntToStr(DB.UserExist('NewEmail@NewEmail.NewEmail')));
end;

procedure TMainForm.MenuItem17Click(Sender: TObject);
// Добавление друга
begin
  Memo.Lines.Add(BoolToStr(DB.AddFriend(1, 'Friend', 'Friend@Friend.Friend'), 'Создан', 'Ошибка'));
  if OpenDialog.Execute then
    Memo.Lines.Add(BoolToStr(DB.AddFriend(1, 'Friend2', 'Friend2@Friend.Friend', OpenDialog.FileName), 'Создан', 'Ошибка'));
end;

procedure TMainForm.MenuItem18Click(Sender: TObject);
// Удаление друга
begin
  Memo.Lines.Add(BoolToStr(DB.RemoveFriend(1, 2), 'Удален 2', 'Ошибка'));
end;

procedure TMainForm.MenuItem20Click(Sender: TObject);
// Получить информацию о друге
begin
  Memo.Lines.Add(DB.GetFriendEmail(1, 1));
  Memo.Lines.Add(DB.GetFriendNickName(1, 1));
end;

procedure TMainForm.MenuItem21Click(Sender: TObject);
// Установить информацию о друге
begin
  Memo.Lines.Add(BoolToStr(DB.SetFriendEmail(1, 1, 'test@friend.ru'), 'Установлен новый email', 'ошибка'));
  Memo.Lines.Add(BoolToStr(DB.SetFriendNickName(1, 1, 'nick@friend'), 'Установлен новое имя пользователя', 'ошибка'));
end;

procedure TMainForm.MenuItem22Click(Sender: TObject);
// Добавить сообщения
var
  Message: TMemoryStream;
  BlowFishKey: TMemoryStream;
  PrivateKey: TMemoryStream;
  OpenKey: TMemoryStream;
begin
  Message := TMemoryStream.Create;
  BlowFishKey := TMemoryStream.Create;
  PrivateKey := TMemoryStream.Create;
  OpenKey := TMemoryStream.Create;
  try
    DB.AddMessage(1, 1, TMsgDirection.mdoutgoingMsg, TMsgType.mtAddFriend, Message, OpenKey, PrivateKey, BlowFishKey);
    DB.AddMessage(1, 1, TMsgDirection.mdoutgoingMsg, TMsgType.mtExchangeKey, Message, OpenKey, PrivateKey, BlowFishKey);
    DB.AddMessage(1, 1, TMsgDirection.mdoutgoingMsg, TMsgType.mtMessage, Message, OpenKey, PrivateKey, BlowFishKey);
    DB.AddMessage(1, 1, TMsgDirection.mdIncomingMsg, TMsgType.mtAddFriend, Message, OpenKey, PrivateKey, BlowFishKey);
    DB.AddMessage(1, 1, TMsgDirection.mdIncomingMsg, TMsgType.mtExchangeKey, Message, OpenKey, PrivateKey, BlowFishKey);
    DB.AddMessage(1, 1, TMsgDirection.mdIncomingMsg, TMsgType.mtMessage, Message, OpenKey, PrivateKey, BlowFishKey);

  finally
    Message.Free;
    BlowFishKey.Free;
    PrivateKey.Free;
    OpenKey.Free;
  end;
  Memo.Lines.Add('Операция выполнена');
end;

procedure TMainForm.MenuItem23Click(Sender: TObject);
// Удалить сообщения
begin
  Memo.Lines.Add(BoolToStr(DB.RemoveMessage(1, 1, 1), 'удалено', 'ошибка'));
end;

procedure TMainForm.MenuItem24Click(Sender: TObject);
// Получить информацию о сообщении
begin
  Memo.Lines.Add(DateTimeToStr(DB.GetMessageDate(1, 1, 2)));
end;

end.
