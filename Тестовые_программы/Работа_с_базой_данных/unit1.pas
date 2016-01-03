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
    MenuItemGetCountMsgs: TMenuItem;
    MenuItemGetFriendCount: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemGetUsersCount: TMenuItem;
    MenuItemDeleteUser: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItemSetNewUserInfo: TMenuItem;
    MenuItemFindUser: TMenuItem;
    MenuItemFriends: TMenuItem;
    MenuItemMsgs: TMenuItem;
    MenuItemNewFriend: TMenuItem;
    MenuItemRemoveFriend: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItemUsers: TMenuItem;
    MenuItemGetFriendInfo: TMenuItem;
    MenuItemSetFriendInfo: TMenuItem;
    MenuItemNewMsg: TMenuItem;
    MenuItemRemoveMsg: TMenuItem;
    MenuItemGetMsgInfo: TMenuItem;
    MenuItemOpenDB: TMenuItem;
    MenuItemCreateDB: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItemNewUser: TMenuItem;
    MenuItemSetUserAvatar: TMenuItem;
    MenuItemGetUserAvatar: TMenuItem;
    MenuItemGetUsersInfo: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemGetCountMsgsClick(Sender: TObject);
    procedure MenuItemGetFriendCountClick(Sender: TObject);
    procedure MenuItemGetUsersCountClick(Sender: TObject);
    procedure MenuItemDeleteUserClick(Sender: TObject);
    procedure MenuItemSetNewUserInfoClick(Sender: TObject);
    procedure MenuItemFindUserClick(Sender: TObject);
    procedure MenuItemNewFriendClick(Sender: TObject);
    procedure MenuItemRemoveFriendClick(Sender: TObject);
    procedure MenuItemGetFriendInfoClick(Sender: TObject);
    procedure MenuItemSetFriendInfoClick(Sender: TObject);
    procedure MenuItemNewMsgClick(Sender: TObject);
    procedure MenuItemRemoveMsgClick(Sender: TObject);
    procedure MenuItemGetMsgInfoClick(Sender: TObject);
    procedure MenuItemCreateDBClick(Sender: TObject);
    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemNewUserClick(Sender: TObject);
    procedure MenuItemSetUserAvatarClick(Sender: TObject);
    procedure MenuItemGetUserAvatarClick(Sender: TObject);
    procedure MenuItemGetUsersInfoClick(Sender: TObject);
    procedure MenuItemOpenDBClick(Sender: TObject);
  private
    procedure SetLog(AValue: string);
    { private declarations }
  public
    { public declarations }
    db: TCustomDataBase;
    property Log: string write SetLog;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  db := TCustomDataBase.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DB.Free;
end;

procedure TMainForm.MenuItemCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.SetLog(AValue: string);
// Заносим в журнал данные или рисуем линию
var
  buf: string;
  i: integer;
begin
  Buf := '';
  if AnsiLowerCase(AValue) = 'nl' then
  begin
    for i := 1 to 80 do
      Buf += '*';
    Memo.Lines.Add(Buf);
  end
  else
    Memo.Lines.Add(AValue);
end;

procedure TMainForm.MenuItemGetFriendCountClick(Sender: TObject);
// Получить количество друзей
begin
  Log := 'nl';
  Log := 'ПОЛУЧАЕМ КОЛИЧЕСТВО ДРУЗЕЙ ПОЛЬЗОВАТЕЛЯ №1';
  Log := 'nl';
  if DB.GetFriendsCount(1) <> INVALID_VALUE then
    Log := 'Количество друзей: ' + IntToStr(DB.GetFriendsCount(1))
  else
    Log := 'Ошибка получения количества друзей';
  Log := '';
end;

procedure TMainForm.MenuItemNewUserClick(Sender: TObject);
// Создание нового пользователя
begin
  Log := 'nl';
  Log := 'ДОБАВЛЕНИЕ НОВОГО ПОЛЬЗОВАТЕЛЯ В БД БЕЗ АВАТАРКИ';
  Log := 'nl';
  if DB.AddUser('Test1', 'pass1', 'Test1@Email.ru') then
  begin
    Log := 'Пользователь создан:';
    Log := #9 + 'Имя: ' + DB.GetUserNickName(DB.GetUsersCount);
    Log := #9 + 'Мыло:' + DB.GetUserEMail(DB.GetUsersCount);
    Log := #9 + 'Хэш: ' + DB.GetUserPasswordHash(DB.GetUsersCount);
  end
  else
    Log := 'Ошибка создания нового пользователя';
  Log := '';
  Log := 'nl';
  Log := 'ДОБАВЛЕНИЕ НОВОГО ПОЛЬЗОВАТЕЛЯ В БД С АВАТАРКОЙ';
  Log := 'nl';
  if OpenDialog.Execute then
    if DB.AddUser('Test2', 'pass2', 'Test2@Email.ru', OpenDialog.FileName) then
    begin
      Log := 'Пользователь создан:';
      Log := #9 + 'Имя: ' + DB.GetUserNickName(DB.GetUsersCount);
      Log := #9 + 'Мыло:' + DB.GetUserEMail(DB.GetUsersCount);
      Log := #9 + 'Хэш: ' + DB.GetUserPasswordHash(DB.GetUsersCount);
    end
    else
      Log := 'Ошибка создания нового пользователя';
  Log := '';
end;

procedure TMainForm.MenuItemSetUserAvatarClick(Sender: TObject);
// Установить новую аватарку
begin
  Log := 'nl';
  Log := 'УСТАНОВКА ПОЛЬЗОВАТЕЛЮ №1 АВАТАРКИ';
  Log := 'nl';
  if OpenDialog.Execute then
    Log := BoolToStr(DB.SetUserAvatar(1, OpenDialog.FileName), 'Установлена аватарка: ' + OpenDialog.FileName, 'Ошибка установки аватарки');
  Log := '';
end;

procedure TMainForm.MenuItemGetUserAvatarClick(Sender: TObject);
// Сохранить аватарку пользователя
var
  MemoryStream: TMemoryStream;
begin
  Log := 'nl';
  Log := 'СОХРАНЕНИЕ В ФАЙЛ АВАТАРКИ ПОЛЬЗОВАТЕЛЯ №2';
  Log := 'nl';
  if SaveDialog.Execute then
  begin
    MemoryStream := TMemoryStream.Create;
    if DB.SaveUserAvatarToStream(2, MemoryStream) then
      Log := 'Аватарка сохранена успешно: ' + SaveDialog.FileName
    else
      Log := 'Ошибка сохранения аватарки в файл';
    MemoryStream.SaveToFile(SaveDialog.FileName);
    MemoryStream.Free;
  end;
  Log := '';
end;

procedure TMainForm.MenuItemGetUsersInfoClick(Sender: TObject);
// Получаем информацию о пользователе
var
  EMail: string;
  PassHash: string;
  Nick: string;
  i: integer;
begin
  Log := 'nl';
  Log := 'ПОЛУЧИТЬ ИНФОРМАЦИЮ О ПОЛЬЗОВАТЕЛЯХ';
  Log := 'nl';
  if DB.GetUsersCount <> INVALID_VALUE then
    for i := 1 to DB.GetUsersCount do
    begin
      DB.GetUserInfo(i, Nick, PassHash, EMail);
      Log := 'ID:   ' + IntToStr(i);
      Log := 'Имя:  ' + Nick;
      Log := 'Хэш:  ' + PassHash;
      Log := 'Мыло: ' + EMail;
      Log := '';
    end;
end;

procedure TMainForm.MenuItemOpenDBClick(Sender: TObject);
// Открытие БД
begin
  Log := 'nl';
  Log := 'ОТКРЫТИЕ БД';
  Log := 'nl';
  if OpenDialog.Execute then
    Log := BoolToStr(DB.OpenDataBase(OpenDialog.FileName), 'Смог открыть БД: ' + OpenDialog.FileName, 'Ошибка открытия БД');
  Log := '';
end;

procedure TMainForm.MenuItemCreateDBClick(Sender: TObject);
// создание БД
begin
  Log := 'nl';
  Log := 'СОЗДАНИЕ БД';
  Log := 'nl';
  if SaveDialog.Execute then
    Log := BoolToStr(DB.CreateDataBase(SaveDialog.FileName), 'БД создана: ' + SaveDialog.FileName, 'Ошибка создания БД');
  Log := '';
end;

procedure TMainForm.MenuItemGetUsersCountClick(Sender: TObject);
// Количество пользователей
begin
  Log := 'nl';
  Log := 'КОЛИЧЕСТВО ПОЛЬЗОВАТЕЛЕЙ В БД';
  Log := 'nl';
  LOG := 'Пользователей в БД: ' + IntToStr(DB.GetUsersCount);
  Log := '';
end;

procedure TMainForm.MenuItemDeleteUserClick(Sender: TObject);
// Удаляем друга
begin
  Log := 'nl';
  Log := 'УДАЛЯЕМ ПОЛЬЗОВАТЕЛЯ №1 В БД';
  Log := 'nl';
  Log := BoolToStr(DB.RemoveUser(1), 'Пользователь удалён', 'Ошибка удаления пользователея');
  Log := '';
end;

procedure TMainForm.MenuItemSetNewUserInfoClick(Sender: TObject);
// Изменяем данные пользователя
begin
  Log := 'nl';
  Log := 'ИЗМЕНЯЕМ ДАННЫЕ ПОЛЬЗОВАТЕЛЯ №2 В БД';
  Log := 'nl';
  Log := 'Текущая информация пользователя №2:';
  Memo.Lines.Add(DB.GetUserNickName(2));
  Memo.Lines.Add(DB.GetUserEMail(2));
  Memo.Lines.Add(DB.GetUserPasswordHash(2));
  Log := 'Изменяем информацию о пользователе: ';
  if DB.SetUserEMail(2, 'NewEmail@NewEmail.NewEmail') and DB.SetUserNickName(2, 'NewNickName') then
    Log := 'Данные пользователя успешно изменены: '
  else
    Log := 'Ошибка изменения информации о пользователе:';
  Memo.Lines.Add(DB.GetUserNickName(2));
  Memo.Lines.Add(DB.GetUserEMail(2));
  Memo.Lines.Add(DB.GetUserPasswordHash(2));
  Log := '';
end;

procedure TMainForm.MenuItemFindUserClick(Sender: TObject);
// Узнать наличие пользователя
begin
  Log := 'nl';
  Log := 'ИЩЕМ ПОЛЬЗОВАТЕЛЯ В БД ПО НЕКОЙ СТРОКЕ';
  Log := 'nl';
  Log := 'Ищем пользователя с мылом - NewEmail@NewEmail.NewEmail';
  Log := 'ID: ' + IntToStr(DB.UserExist('NewEmail@NewEmail.NewEmail'));
  Log := 'Ищем пользователя с именем - Test2';
  Log := 'ID: ' + IntToStr(DB.UserExist('Test2'));
  Log := '';
end;

procedure TMainForm.MenuItemNewFriendClick(Sender: TObject);
// Добавление друга
begin
  Log := 'nl';
  Log := 'ДОБАВЛЯЕМ НОВОГО ДРУГА БЕЗ АВАТАРКИ';
  Log := 'nl';
  if DB.AddFriend(1, 'Friend', 'Friend@Friend.Friend') then
  begin
    Log := 'Друг создан:';
    Log := #9 + 'Имя: ' + DB.GetFriendNickName(1, DB.GetUsersCount);
    Log := #9 + 'Мыло:' + DB.GetFriendEmail(1, DB.GetUsersCount);
  end
  else
    Log := 'Ошибка создания нового друга';
  Log := '';
  Log := 'nl';
  Log := 'ДОБАВЛЯЕМ НОВОГО ДРУГА С АВАТАРКОЙ';
  Log := 'nl';
  if OpenDialog.Execute then
    if DB.AddFriend(1, 'Friend', 'Friend@Friend.Friend', OpenDialog.FileName) then
    begin
      Log := 'Друг создан:';
      Log := #9 + 'Имя: ' + DB.GetFriendNickName(1, DB.GetUsersCount);
      Log := #9 + 'Мыло:' + DB.GetFriendEmail(1, DB.GetUsersCount);
    end
    else
      Log := 'Ошибка создания нового друга';
  Log := '';
end;

procedure TMainForm.MenuItemRemoveFriendClick(Sender: TObject);
// Удаление друга
begin
  Log := 'nl';
  Log := 'УДАЛЯЕМ ДРУГА №2 ПОЛЬЗОВАТЕЛЯ № 1 В БД';
  Log := 'nl';
  Log := BoolToStr(DB.RemoveFriend(1, 2), 'Друг №2 удалён из БД', 'Ошибка удаления друга из БД');
  Log := '';
end;

procedure TMainForm.MenuItemGetFriendInfoClick(Sender: TObject);
// Получить информацию о друге
var
  i: integer;
begin
  Log := 'nl';
  Log := 'ПОЛУЧАЕМ ИНФОРМАЦИЮ О ДРУЗЬЯХ ПОЛЬЗОВАТЕЛЯ №1';
  Log := 'nl';
  if DB.GetFriendsCount(1) <> INVALID_VALUE then
    for i := 1 to DB.GetFriendsCount(1) do
    begin
      Log := 'id:   ' + IntToStr(i);
      Log := 'Имя:  ' + DB.GetFriendNickName(1, i);
      Log := 'Мыло: ' + DB.GetFriendEmail(1, i);
      Log := '';
    end;
end;

procedure TMainForm.MenuItemSetFriendInfoClick(Sender: TObject);
// Установить информацию о друге
begin
  Log := 'nl';
  Log := 'УСТАНАВЛИВАЕМ НОВЫЕ ДАННЫЕ ДРУГУ №1 ПОЛЬЗОВАТЕЛЯ №1';
  Log := 'nl';
  Log := 'Старые данные:';
  Log := 'Имя:  ' + DB.GetFriendNickName(1, 1);
  Log := 'Мыло: ' + DB.GetFriendEmail(1, 1);
  Log := 'Установка новых данных:';
  if not DB.SetFriendEmail(1, 1, 'test@friend.ru') then
    Log := 'Ошибка установки нового мыла';
  if not DB.SetFriendNickName(1, 1, 'nick@friend') then
    Log := 'Ошибка установки нового имени';
  Log := 'Новые данные:';
  Log := 'Имя:  ' + DB.GetFriendNickName(1, 1);
  Log := 'Мыло: ' + DB.GetFriendEmail(1, 1);
  Log := '';
end;

procedure TMainForm.MenuItemNewMsgClick(Sender: TObject);
// Добавить сообщения
var
  Message: TMemoryStream;
  BlowFishKey: TMemoryStream;
  PrivateKey: TMemoryStream;
  OpenKey: TMemoryStream;
  Status: boolean;
begin
  Log := 'nl';
  Log := 'СОЗДАНИЕ СООБЩЕНИЙ МЕЖДУ ДРУГОМ №1 И ПОЛЬЗОВАТЕЛЯМ №1';
  Log := 'nl';
  Message := TMemoryStream.Create;
  Message.LoadFromFile(Application.ExeName + '.lpi');
  BlowFishKey := TMemoryStream.Create;
  BlowFishKey.LoadFromFile(Application.ExeName + '.lpi');
  PrivateKey := TMemoryStream.Create;
  PrivateKey.LoadFromFile(Application.ExeName + '.lpi');
  OpenKey := TMemoryStream.Create;
  OpenKey.LoadFromFile(Application.ExeName + '.lpi');
  try
    Status := True;
    Status := Status and DB.AddMessage(1, 1, TMsgDirection.mdoutgoingMsg, TMsgType.mtAddFriend, Message, OpenKey, PrivateKey, BlowFishKey);
    Status := Status and DB.AddMessage(1, 1, TMsgDirection.mdoutgoingMsg, TMsgType.mtExchangeKey, Message, OpenKey, PrivateKey, BlowFishKey);
    Status := Status and DB.AddMessage(1, 1, TMsgDirection.mdoutgoingMsg, TMsgType.mtMessage, Message, OpenKey, PrivateKey, BlowFishKey);
    Status := Status and DB.AddMessage(1, 1, TMsgDirection.mdIncomingMsg, TMsgType.mtAddFriend, Message, OpenKey, PrivateKey, BlowFishKey);
    Status := Status and DB.AddMessage(1, 1, TMsgDirection.mdIncomingMsg, TMsgType.mtExchangeKey, Message, OpenKey, PrivateKey, BlowFishKey);
    Status := Status and DB.AddMessage(1, 1, TMsgDirection.mdIncomingMsg, TMsgType.mtMessage, Message, OpenKey, PrivateKey, BlowFishKey);
  finally
    Message.Free;
    BlowFishKey.Free;
    PrivateKey.Free;
    OpenKey.Free;
  end;
  if Status then
    Log := 'Все пользователи созданы успешно'
  else
    Log := 'Ошибка создания новых пользователей';
  Log := '';
end;

procedure TMainForm.MenuItemRemoveMsgClick(Sender: TObject);
// Удалить сообщения
begin
  Log := 'nl';
  Log := 'УДАЛЯЕМ СООБЩЕНИЕ №3 ДРУГА №2 ПОЛЬЗОВАТЕЛЯ № 1 В БД';
  Log := 'nl';
  Log := BoolToStr(DB.RemoveMessage(1, 1, 1), 'Сообщение успешно удалено', 'Ошибка удаления сообщения');
  Log := '';
end;

procedure TMainForm.MenuItemGetMsgInfoClick(Sender: TObject);
// Получить информацию о сообщении
var
  StringStream: TStringStream;
begin
  Log := 'nl';
  Log := 'ПОЛУЧИТЬ ИНФОРМАЦИЮ О СООБЩЕНИИ №1 ДРУГА №2 ПОЛЬЗОВАТЕЛЯ № 1 В БД';
  Log := 'nl';
  case DB.GetMessageType(1, 1, 1) of
    mtAddFriend: Log := 'Тип сообщения: Добавление в друзья';
    mtExchangeKey: Log := 'Тип сообщения: Обмен ключами';
    mtMessage: Log := 'Тип сообщения: Сообщение';
  end;
  if DB.GetMessageDirection(1, 1, 1) = mdIncomingMsg then
    Log := 'Входящее сообщение'
  else
    Log := 'Исходящее сообщение';
  Log := 'Дата сообщения: ' + DateTimeToStr(DB.GetMessageDate(1, 1, 2));
  StringStream:= TStringStream.Create('');
  db.GetMessage(1, 1, 1, StringStream);
  Log := 'Текст сообщения: ';
  Log := StringStream.DataString;
  Log := '';
end;

procedure TMainForm.MenuItemGetCountMsgsClick(Sender: TObject);
// Получить количество сообщений
begin
  Log := 'nl';
  Log := 'КОЛИЧЕСТВО СООБЩЕНИЙ В БД';
  Log := 'nl';
  LOG := 'Сообщений в БД: ' + IntToStr(DB.GetMessagesCount(1, 1));
  Log := '';
end;

end.
