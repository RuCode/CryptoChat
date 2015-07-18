unit Test;

{$mode objfpc}{$H+}

interface

uses
Classes, Controls, Dialogs, FileUtil, Forms, Graphics, MailCrypt, MainCrypt, Menus, StdCtrls,
SysUtils;

type

{ TTestForm }

TTestForm = class(TForm)
  ImageList          :TImageList;
  MainMenu           :TMainMenu;
  Memo               :TMemo;
  MenuGetMailSubj: TMenuItem;
  MenuDB          :TMenuItem;
  MenuEncodeBf: TMenuItem;
  MenuDecodeBf: TMenuItem;
  MenuGetMailCount: TMenuItem;
  MenuItem1: TMenuItem;
  MenuItem2: TMenuItem;
  MenuItem3: TMenuItem;
  MenuItem4: TMenuItem;
  MenuReadMail: TMenuItem;
  MenuSep5: TMenuItem;
  MenuEncodeBase64: TMenuItem;
  MenuDecodeBase64: TMenuItem;
  MenuGetMailTitle: TMenuItem;
  MenuGetAddrFrom: TMenuItem;
  MenuSep6: TMenuItem;
  MenuGetAddrTo: TMenuItem;
  MenuGet: TMenuItem;
  MenuSep9: TMenuItem;
  MenuGetCountAttach: TMenuItem;
  MenuSaveAttach: TMenuItem;
  MenuEdit: TMenuItem;
  MenuItem23: TMenuItem;
  MenuSep10: TMenuItem;
  MenuClearLog: TMenuItem;
  MenuItem26: TMenuItem;
  MenuSep8: TMenuItem;
  MenuMakeTar: TMenuItem;
  MenuMakeFiles: TMenuItem;
  MenuExtractTar: TMenuItem;
  MenuGetLastOpenKey: TMenuItem;
  MenuReadFiles: TMenuItem;
  MenuSep7: TMenuItem;
  MenuMakeFirst: TMenuItem;
  MenuReadFirst: TMenuItem;
  MenuMailConnect: TMenuItem;
  MenuMsgXid: TMenuItem;
  MenuSep1          :TMenuItem;
  MenuGetUserInfo    :TMenuItem;
  MenuGetCountUsers  :TMenuItem;
  MenuSep2          :TMenuItem;
  MenuAddFriend          :TMenuItem;
  MenuFriendCount    :TMenuItem;
  MenuCreateTbls     :TMenuItem;
  MenuFriendInfo     :TMenuItem;
  MenuDeleteFriend   :TMenuItem;
  MenuSep3: TMenuItem;
  MenuCrypto          :TMenuItem;
  MenuMail          :TMenuItem;
  MenuTest          :TMenuItem;
  MenuCountMsg: TMenuItem;
  MenuGenRsaKeys: TMenuItem;
  MenuCode: TMenuItem;
  MenuDecode: TMenuItem;
  MenuSep4: TMenuItem;
  MenuGenBfPwd: TMenuItem;
  MenuReadMsg: TMenuItem;
  MenuNewMessage: TMenuItem;
  MenuUpdateFriend   :TMenuItem;
  MenuUpdateUserInfo :TMenuItem;
  MenuUserExist      :TMenuItem;
  MenuLoginUser      :TMenuItem;
  MenuRegister       :TMenuItem;
  MenuCreateDB       :TMenuItem;
  procedure FormCreate(Sender :TObject);
  procedure FormDestroy(Sender :TObject);
  procedure MemoChange(Sender: TObject);
  procedure MenuCodeClick(Sender: TObject);
  procedure MenuCountMsgClick(Sender: TObject);
  procedure MenuCreateDBClick(Sender :TObject);
  procedure MenuCreateTblsClick(Sender :TObject);
  procedure MenuDecodeClick(Sender: TObject);
  procedure MenuDeleteFriendClick(Sender: TObject);
  procedure MenuFriendCountClick(Sender: TObject);
  procedure MenuFriendInfoClick(Sender: TObject);
  procedure MenuGenRsaKeysClick(Sender: TObject);
  procedure MenuGetCountUsersClick(Sender: TObject);
  procedure MenuGetUserInfoClick(Sender: TObject);
  procedure MenuAddFriendClick(Sender: TObject);
  procedure MenuEncodeBfClick(Sender: TObject);
  procedure MenuDecodeBfClick(Sender: TObject);
  procedure MenuEncodeBase64Click(Sender: TObject);
  procedure MenuDecodeBase64Click(Sender: TObject);
  procedure MenuGetMailCountClick(Sender: TObject);
  procedure MenuItem1Click(Sender: TObject);
  procedure MenuItem26Click(Sender: TObject);
  procedure MenuItem3Click(Sender: TObject);
  procedure MenuItem4Click(Sender: TObject);
  procedure MenuReadMailClick(Sender: TObject);
  procedure MenuGetMailTitleClick(Sender: TObject);
  procedure MenuGetAddrFromClick(Sender: TObject);
  procedure MenuGetAddrToClick(Sender: TObject);
  procedure MenuGetSubjectClick(Sender: TObject);
  procedure MenuGetCountAttachClick(Sender: TObject);
  procedure MenuSaveAttachClick(Sender: TObject);
  procedure MenuClearLogClick(Sender: TObject);
  procedure MenuMakeTarClick(Sender: TObject);
  procedure MenuMakeFilesClick(Sender: TObject);
  procedure MenuExtractTarClick(Sender: TObject);
  procedure MenuGetLastOpenKeyClick(Sender: TObject);
  procedure MenuReadFilesClick(Sender: TObject);
  procedure MenuMakeFirstClick(Sender: TObject);
  procedure MenuReadFirstClick(Sender: TObject);
  procedure MenuMailConnectClick(Sender: TObject);
  procedure MenuGenBfPwdClick(Sender: TObject);
  procedure MenuLoginUserClick(Sender :TObject);
  procedure MenuMsgXidClick(Sender: TObject);
  procedure MenuNewMessageClick(Sender: TObject);
  procedure MenuReadMsgClick(Sender: TObject);
  procedure MenuRegisterClick(Sender :TObject);
  procedure MenuUpdateFriendClick(Sender: TObject);
  procedure MenuUpdateUserInfoClick(Sender: TObject);
  procedure MenuUserExistClick(Sender: TObject);
private
  { private declarations }
public
  { public declarations }
  Crypt    :TMailCrypt;
  procedure Log(AText :string); overload;
  procedure Log(AUser :TUserEntry); overload;
  procedure Log(AFriend :TFriendEntry); overload;
  procedure Log(AMSg :TMessageEntry); overload;
  procedure SortMailList(AMailList: TStringList);
end;

var
TestForm :TTestForm;

implementation

{$R *.lfm}

{ TTestForm }

procedure TTestForm.FormCreate(Sender :TObject);
begin
  Crypt:= TMailCrypt.Create(nil);
end;

procedure TTestForm.FormDestroy(Sender :TObject);
begin
  Crypt.Free;
end;

procedure TTestForm.MemoChange(Sender: TObject);
begin

end;

procedure TTestForm.MenuCodeClick(Sender: TObject);
var
   PathFile, PathOpn: String;
begin
  PathOpn:= InputBox('Ввод', 'Введите имя открытого ключа', 'Public.pub');
  PathFile:= InputBox('Ввод', 'Введите имя файла для шифровки', 'TEXT_FILE');
  Log(Crypt.EncryptFile(PathOpn, PathFile, PathFile+'.enc'));
end;

procedure TTestForm.MenuCountMsgClick(Sender: TObject);
var
   fIndex: String;
begin
   if Crypt.AssignDB then
  begin
     if Crypt.isUserLogin then
     begin
        fIndex:= InputBox('Ввод', 'Введите индекс друга', '');
        if fIndex = '' then exit;
        Log('Количество сообщений: ' + IntToStr(Crypt.GetCountMessage(StrToInt(fIndex))));
     end else Log('Нет вошедших в систему пользователей');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuCreateDBClick(Sender :TObject);
begin
  if Crypt.OpenDB(ExtractFilePath(Application.ExeName)+'DataBase.db3') then
    Log('База данных создана: ' + Crypt.GetDBFilePath)
  else
    Log('Ошибка создания базы данных: ' + Crypt.GetDBFilePath);
end;

procedure TTestForm.MenuCreateTblsClick(Sender :TObject);
begin
  Log('Таблицы БД создаются во время открытия...');
end;

procedure TTestForm.MenuDecodeClick(Sender: TObject);
var
   PathFile, PathPriv: String;
begin
  PathPriv:= InputBox('Ввод', 'Введите имя закрытого ключа', 'Private.pub');
  PathFile:= InputBox('Ввод', 'Введите имя файла для разшифровки', 'TEXT_FILE');
  Log(Crypt.DecryptFile(PathPriv, PathFile, PathFile+'.decode'));
end;

procedure TTestForm.MenuDeleteFriendClick(Sender: TObject);
var
   fIndex: String;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
         fIndex:= InputBox('Ввод', 'Введите индекс друга', '');
         if fIndex = '' then exit;
         if Crypt.DeleteFriend(StrToInt(fIndex)) then
            Log('Удаление друга прошло успешно, индекс: ' + fIndex)
         else
            Log('Удалить друга не получилось, индекс: ' + fIndex)
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuFriendCountClick(Sender: TObject);
begin
  if Crypt.AssignDB then
  begin
     if Crypt.isUserLogin then
     begin
          Log('Количество друзей: ' + IntToStr(Crypt.GetCountFriend));
     end else Log('Нет вошедших в систему пользователей...');
  end else
    Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuFriendInfoClick(Sender: TObject);
var
   fIndex: String;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
         fIndex:= InputBox('Ввод', 'Введите индекс друга', '');
         if fIndex = '' then exit;
         Log(Crypt.Friend[StrToInt(fIndex)]);
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuGenRsaKeysClick(Sender: TObject);
var
   PathSec, PathOpn: String;
begin
  PathSec:= InputBox('Ввод', 'Введите имя секретного ключа', 'Private.pub');
  PathOpn:= InputBox('Ввод', 'Введите имя открытого ключа', 'Public.pub');
  Log(Crypt.GenRsaKeys(PathSec, PathOpn, 2048));
end;

procedure TTestForm.MenuGetCountUsersClick(Sender: TObject);
begin
  if Crypt.AssignDB then
  begin
    Log('Количество пользователей: ' + IntToStr(Crypt.GetCountUsers));
  end else
    Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuGetUserInfoClick(Sender: TObject);
var
  AUser :TUserEntry;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
       AUser:= Crypt.GetUserInfo;
       Log(AUser);
       Log('ID: ' + IntToStr(AUser.ID_USER));
    end else Log('Нет вошедших в систему пользователей...');
  end
  else
    Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuAddFriendClick(Sender: TObject);
var
  AFriend: TFriendEntry;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
       AFriend.NickName := InputBox('Ввод', 'Введите Nick', '');
       if AFriend.NickName = '' then exit;
       AFriend.Email    := InputBox('Ввод', 'Введите адрес почты', '');
       if AFriend.EMail = '' then exit;
       if Crypt.AddFriend(AFriend) then
          Log('Добавлен новый друг с Email: ' + AFriend.Email)
       else
          Log('Не возможно добавить нового друга с Email: ' + AFriend.Email)
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuEncodeBfClick(Sender: TObject);
var
   PathFile, PathOpn: String;
begin
  PathOpn:= InputBox('Ввод', 'Введите имя ключа', 'Password.bf');
  PathFile:= InputBox('Ввод', 'Введите имя файла для шифровки', 'TEXT_FILE');
  Log(Crypt.EncryptFileBf(PathOpn, PathFile, PathFile+'.enc'));
end;

procedure TTestForm.MenuDecodeBfClick(Sender: TObject);
var
   PathFile, PathOpn: String;
begin
  PathOpn:= InputBox('Ввод', 'Введите имя ключа', 'Password.bf');
  PathFile:= InputBox('Ввод', 'Введите имя файла для шифровки', 'TEXT_FILE.enc');
  Log(Crypt.DecryptFileBf(PathOpn, PathFile, PathFile+'.dec'));
end;

procedure TTestForm.MenuEncodeBase64Click(Sender: TObject);
var
   PathFile: String;
begin
  PathFile:= InputBox('Ввод', 'Введите имя файла для кодирования', 'TEXT_FILE');
  Log(Crypt.Base64Encode(PathFile, PathFile+'.base64'));
end;

procedure TTestForm.MenuDecodeBase64Click(Sender: TObject);
var
   PathFile: String;
begin
  PathFile:= InputBox('Ввод', 'Введите имя файла для кодирования', 'TEXT_FILE.base64');
  Log(Crypt.Base64Decode(PathFile, PathFile+'.debase64'));
end;

procedure TTestForm.MenuGetMailCountClick(Sender: TObject);
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
         Log('Количество писем: '+IntToStr(Crypt.GetMailCount));
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuItem1Click(Sender: TObject);
var
   fIndex: String;
   i: Integer;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
      fIndex:= InputBox('Ввод', 'Введите индекс с которого начать загрузку', '');
      if fIndex = '' then exit;
      Log('Индекс'+#9+'От кого'+#9+#9+'Кому'+#9+#9+'Тема');
      for i:= StrToInt(fIndex) to StrToInt(fIndex)+600 do
      begin
        Log(IntToStr(i) + #9 + #9 +
            Crypt.ExtractEmail(Crypt.GetMailFrom(i)) + #9 +
            Crypt.ExtractEmail(Crypt.GetMailTo(i)) + #9 +
            Crypt.GetMailSubject(i));
        Application.ProcessMessages;
      end;
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.SortMailList(AMailList: TStringList);
var
   i, j: Integer;
   ThisDate, NextDate: TDateTime;
begin
  for i:= 0 to AMailList.Count-1 do
      for j:= 0 to AMailList.Count-2 do
      begin
           Crypt.LoadMailFromText(0, AMailList[j]);
           ThisDate:= Crypt.GetMailDate(0);
           Crypt.LoadMailFromText(0, AMailList[j+1]);
           NextDate:= Crypt.GetMailDate(0);
           if ThisDate > NextDate then
              AMailList.Exchange(j, j+1);
      end;
end;

procedure TTestForm.MenuItem26Click(Sender: TObject);
var
   fIndex, fMailFriend: String;
   i: Integer;
   fChat: TStringList;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
      fChat:= TStringList.Create;
      fIndex:= InputBox('Ввод', 'Введите индекс с которого начать загрузку', '');
      if fIndex = '' then exit;
      fMailFriend:= InputBox('Ввод', 'Введите адрес почты с которым показать переписку', '');
      if fMailFriend = '' then exit;
      Log('Индекс'+#9+'Дата'+#9+'От кого'+#9+#9+'Кому'+#9+#9+'Тема');
      for i:= StrToInt(fIndex) to StrToInt(fIndex)+600 do
      begin
        if (SameText(fMailFriend, Crypt.ExtractEmail(Crypt.GetMailFrom(i)))) or
           (SameText(fMailFriend, Crypt.ExtractEmail(Crypt.GetMailTo(i)))) then
        begin
           fChat.Add(Crypt.GetMail(i));
        Log(IntToStr(i) + #9 + #9 +
            DateTimeToStr(Crypt.GetMailDate(i)) + #9 +
            Crypt.ExtractEmail(Crypt.GetMailFrom(i)) + #9 +
            Crypt.ExtractEmail(Crypt.GetMailTo(i)) + #9 +
            Crypt.GetMailSubject(i));
        end;
        Application.ProcessMessages;
      end;
      Log('Всего сообщений: '+IntToStr(fChat.Count - 1));
      SortMailList(fChat);
      for i:= 0 to fChat.Count-1 do
      begin
        Crypt.LoadMailFromText(0, fChat[i]);
        Log(IntToStr(i+1) + #9 + #9 +
                    DateTimeToStr(Crypt.GetMailDate(0)) + #9 +
                    Crypt.ExtractEmail(Crypt.GetMailFrom(0)) + #9 +
                    Crypt.ExtractEmail(Crypt.GetMailTo(0)) + #9 +
                    Crypt.GetMailSubject(0));
      end;
      Log('Всего сообщений: '+IntToStr(fChat.Count - 1));
      fChat.Free;
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuItem3Click(Sender: TObject);
var
   fIndex: String;
begin
  if Crypt.AssignDB then
  begin
      fIndex:= InputBox('Ввод', 'Введите индекс пользователя', '');
      if fIndex = '' then exit;
      Crypt.SetUserImage(StrToInt(fIndex), ExtractFilePath(ParamStr(0))+'Data/def_ava.png');
      Log('Установлен аватар для пользователя № '+fIndex);
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuItem4Click(Sender: TObject);
var
   fIndex: String;
begin
  if Crypt.AssignDB then
  begin
      fIndex:= InputBox('Ввод', 'Введите индекс пользователя', '');
      if fIndex = '' then exit;
      Crypt.GetUserImage(StrToInt(fIndex), ExtractFilePath(ParamStr(0))+'Data/def_ava2.png');
      Log('Установлен аватар для пользователя № '+fIndex);
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuReadMailClick(Sender: TObject);
var
   fIndex: String;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
      fIndex:= InputBox('Ввод', 'Введите индекс сообщения', '');
      if fIndex = '' then exit;
      Log('Тело письма: '#13+#10+Crypt.GetMail(StrToInt(fIndex)));
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuGetMailTitleClick(Sender: TObject);
var
   fIndex: String;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
      fIndex:= InputBox('Ввод', 'Введите индекс сообщения', '');
      if fIndex = '' then exit;
      Log('Тело письма: '#13+#10+Crypt.GetMailHead(StrToInt(fIndex)));
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuGetAddrFromClick(Sender: TObject);
var
   fIndex: String;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
      fIndex:= InputBox('Ввод', 'Введите индекс сообщения', '');
      if fIndex = '' then exit;
      Log('Письмо отправлено от: '#13+#10+Crypt.GetMailFrom(StrToInt(fIndex)));
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuGetAddrToClick(Sender: TObject);
var
   fIndex: String;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
      fIndex:= InputBox('Ввод', 'Введите индекс сообщения', '');
      if fIndex = '' then exit;
      Log('Письмо отправлено: '#13+#10+Crypt.GetMailTo(StrToInt(fIndex)));
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuGetSubjectClick(Sender: TObject);
var
   fIndex: String;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
      fIndex:= InputBox('Ввод', 'Введите индекс сообщения', '');
      if fIndex = '' then exit;
      Log('Письмо отправлено: '#13+#10+Crypt.GetMailSubject(StrToInt(fIndex)));
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuGetCountAttachClick(Sender: TObject);
var
   fIndex: String;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
      fIndex:= InputBox('Ввод', 'Введите индекс сообщения', '');
      if fIndex = '' then exit;
      Log('Количество вложений: '#13+#10+IntToStr(1+Crypt.GetAttachCount(StrToInt(fIndex))));
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuSaveAttachClick(Sender: TObject);
var
   pMailIndex, pIndex, Path: String;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
      pMailIndex:= InputBox('Ввод', 'Введите индекс сообщения', '');
      if pMailIndex = '' then exit;
      pIndex:= InputBox('Ввод', 'Введите индекс вложения', '');
      if pIndex = '' then exit;
      Path:= '/tmp/'+Crypt.GetAttachName(StrToInt(pMailIndex), StrToInt(pIndex)-1);
      Log('Сохраняю вложения в: '+Path);
      Crypt.SaveAttachToFile(StrToInt(pMailIndex), StrToInt(pIndex), Path);
      Log('Сохранено');
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuClearLogClick(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TTestForm.MenuMakeTarClick(Sender: TObject);
begin
  Log(Crypt.MakeBz2(InputBox('Ввод', 'Введите путь к файлу для архивирования', 'Launch'), 'backup-bzip2.tar.bz2'));
end;

procedure TTestForm.MenuMakeFilesClick(Sender: TObject);
begin
  if Crypt.MakeMail(ExtractFilePath(Application.ExeName)+'tmp/',
           'Тут у нас текст',
           ExtractFilePath(Application.ExeName)+'Launch.ico', // Файлы
           0) then
           Log('Файлы готовы');
end;

procedure TTestForm.MenuExtractTarClick(Sender: TObject);
begin
  Log(Crypt.ExtractBz2(InputBox('Ввод', 'Введите путь к архиву', 'backup-bzip2.tar.bz2'), './tmp'));
end;

procedure TTestForm.MenuGetLastOpenKeyClick(Sender: TObject);
var
   fIndex: String;
begin
   if Crypt.AssignDB then
  begin
     if Crypt.isUserLogin then
     begin
        fIndex:= InputBox('Ввод', 'Введите индекс друга', '');
        if fIndex = '' then exit;
        Log('OpenKey: ' + Crypt.GetLastOpenKey(StrToInt(fIndex)));
     end else Log('Нет вошедших в систему пользователей...');
  end else
    Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuReadFilesClick(Sender: TObject);
begin
  if Crypt.AssignDB then
  begin
     if Crypt.isUserLogin then
     begin
          if Crypt.ReadMail(ExtractFilePath(Application.ExeName)+'tmp/', 0) then
          begin
               Log('Файлы прочитаны');
               Log(Crypt.GetMessageEntry(Crypt.GetCountMessage(0)-1, 0).Message);
          end;
     end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuMakeFirstClick(Sender: TObject);
var
   fIndex: String;
begin
   if Crypt.AssignDB then
  begin
     if Crypt.isUserLogin then
     begin
        fIndex:= InputBox('Ввод', 'Введите индекс друга', '');
        if fIndex = '' then exit;
        Crypt.MakeFirstMail(ExtractFilePath(Application.ExeName)+'tmp/', StrToInt(fIndex));
     end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuReadFirstClick(Sender: TObject);
var
   fIndex: String;
begin
   if Crypt.AssignDB then
  begin
     if Crypt.isUserLogin then
     begin
        fIndex:= InputBox('Ввод', 'Введите индекс друга', '');
        if fIndex = '' then exit;
        Crypt.ReadFirstMail(ExtractFilePath(Application.ExeName)+'tmp/', StrToInt(fIndex));
     end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuMailConnectClick(Sender: TObject);
begin
   if Crypt.AssignDB then
  begin
     if Crypt.isUserLogin then
     begin
       Crypt.Host:= 'pop.yandex.ru';
       Log(BoolToStr(Crypt.ConnectMail, 'Подключение прошло успешно', 'Ошибка соединения с сервером'));
     end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuGenBfPwdClick(Sender: TObject);
begin
  Log(Crypt.GenBfPassword(InputBox('Ввод', 'Введите путь сохранения пароля', 'Password.bf')));
end;

procedure TTestForm.MenuLoginUserClick(Sender :TObject);
var
  AUser :TUserEntry;
begin
  if Crypt.AssignDB then
  begin
    AUser.Email    := InputBox('Ввод', 'Введите адрес почты', '');
    AUser.HashPwd  := PasswordBox('Ввод', 'Введите пароль');
    if Crypt.LoginUser(AUser.Email, AUser.HashPwd) then
      Log('Вход выполнен для: ' + AUser.Email)
    else
      Log('Ошибка входа проверьте правильность email и пароля: ' + AUser.Email);
  end
  else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuMsgXidClick(Sender: TObject);
var
   fIndex: String;
begin
   if Crypt.AssignDB then
  begin
     if Crypt.isUserLogin then
     begin
        fIndex:= InputBox('Ввод', 'Введите индекс друга', '');
        if fIndex = '' then exit;
        Log('Максимальный XID: ' + IntToStr(Crypt.GetMaxXid(StrToInt(fIndex))));
     end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuNewMessageClick(Sender: TObject);
var
  Amsg: TMessageEntry;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
      Amsg.ID_FRIEND:= 1;
      Amsg.ID_USER:= 1;
      Amsg.Date:= Now;
      Amsg.Files := 'FILES';
      Amsg.Message := 'MESSAGE';
      Amsg.IsMyMsg := True;
      Amsg.OpenKey:= 'OPEN-KEY';
      Amsg.SecretKey:= 'SECRET-KEY';
      Amsg.BFKey:= 'SECRET-KEY';
      Amsg.XID:= 1;
      if Crypt.AddMessage(Amsg) then
         log('Сообщение добавлено в БД: '+Amsg.Message)
      else
         log('Ошибка добавления сообщения в БД: '+Amsg.Message);
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuReadMsgClick(Sender: TObject);
var
  Index, AFriendID: String;
  AMessage: TMessageEntry;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
       Index := InputBox('Ввод', 'Введите индекс сообщения', '');
       if Index = '' then exit;
       AFriendID := InputBox('Ввод', 'Введите ID друга', '');
       if AFriendID = '' then exit;
       AMessage:= Crypt.GetMessageEntry(StrToInt(Index), StrToInt(AFriendID));
       Log(AMessage);
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuRegisterClick(Sender :TObject);
var
  AUser :TUserEntry;
begin
  if Crypt.AssignDB then
  begin
    AUser.NickName := InputBox('Ввод', 'Введите Nick', '');
    if AUser.NickName = '' then exit;
    AUser.HashPwd  := InputBox('Ввод', 'Введите пароль', '');
    if AUser.HashPwd = '' then exit;
    AUser.Email    := InputBox('Ввод', 'Введите адрес почты', '');
    if AUser.EMail = '' then exit;
    if Crypt.AddUser(AUser) then
      Log(AUser)
    else
      Log('Ошибка создания нового пользователя');
  end
  else
    Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuUpdateFriendClick(Sender: TObject);
var
  AFriend :TFriendEntry;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
         AFriend.ID_FRIEND := StrToInt(InputBox('Ввод', 'Введите ID_FRIEND', ''));
         AFriend.NickName := InputBox('Ввод', 'Введите Nick', '');
         if AFriend.NickName = '' then exit;
         AFriend.Email    := InputBox('Ввод', 'Введите адрес почты', '');
         if AFriend.EMail = '' then exit;
         if Crypt.UpdateFriend(AFriend) then
            Log(AFriend)
         else
            Log('Ошибка создания нового пользователя');
    end else Log('Нет вошедших в систему пользователей...');
  end else Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuUpdateUserInfoClick(Sender: TObject);
var
  AUser :TUserEntry;
begin
  if Crypt.AssignDB then
  begin
    if Crypt.isUserLogin then
    begin
         AUser.NickName := InputBox('Ввод', 'Введите Nick', '');
         if AUser.NickName = '' then exit;
         AUser.Email    := InputBox('Ввод', 'Введите адрес почты', '');
         if AUser.EMail = '' then exit;
         if Crypt.UpdateUser(AUser) then
            Log(AUser)
         else
            Log('Ошибка создания нового пользователя');
    end else  Log('Нет вошедших в систему пользователей...');
  end
  else
    Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.MenuUserExistClick(Sender: TObject);
var
  AEMail: String;
begin
  if Crypt.AssignDB then
  begin
     AEMail:= InputBox('Ввод', 'Введите адрес почты', '');
     if AEMail = '' then exit;
     if Crypt.ExistUser(AEMail) then
        Log('Пользователь существует: ' + AEMail)
     else
        Log('Пользователя не существует: ' + AEMail)
  end else
    Log('Ошибка получения доступа к базе данных');
end;

procedure TTestForm.Log(AText :string);
begin
  Memo.Lines.Add('');
  Memo.Lines.Add('# ' + AText);
end;

procedure TTestForm.Log(AUser :TUserEntry);
begin
  Log('Пользователь:' + #13 + #10 +
  'NickName:   ' + AUser.NickName + #13 + #10 +
  'Почта:      ' + AUser.Email + #13 + #10 +
  'Пароль:     ' + AUser.HashPwd);
end;

procedure TTestForm.Log(AFriend: TFriendEntry);
begin
  Log('Друг:' + #13 + #10 +
  'ID_USER:    ' + IntToStr(AFriend.ID_USER) + #13 + #10 +
  'ID_FRIEND:  ' + IntToStr(AFriend.ID_FRIEND) + #13 + #10 +
  'UUID:       ' + AFriend.UUID + #13 + #10 +
  'NickName:   ' + AFriend.NickName + #13 + #10 +
  'Почта:      ' + AFriend.Email);
end;

procedure TTestForm.Log(AMSg: TMessageEntry);
begin
  Log('Сообщение:' + #13 + #10 +
  'ID_FRIEND:    ' + IntToStr(AMSg.ID_FRIEND) + #13 + #10 +
  'ID_USER:      ' + IntToStr(AMSg.ID_USER) + #13 + #10 +
  'Дата:         ' + DateTimeToStr(AMSg.Date) + #13 + #10 +
  'XID:          ' + IntToStr(AMSg.XID) + #13 + #10 +
  'Это моё :     ' + BoolToStr(AMSg.IsMyMsg, 'Да', 'Нет') + #13 + #10 +
  'Открытый ключ:' + AMSg.OpenKey + #13 + #10 +
  'Закрытый ключ:' + AMSg.SecretKey + #13 + #10 +
  'Сообщение:    ' + AMSg.Message + #13 + #10 +
  'Файлы:        ' + AMSg.Files);
end;

end.
