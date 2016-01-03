unit CryptFrame;

{$mode objfpc}{$H+}

interface

uses
Classes, Controls, CryptMessage, Cryptor, Dialogs, ExtCtrls, FileUtil, Forms, Graphics,
Menus, StdCtrls, SysUtils, UserControl;

type
TFiles = pointer;

type

{ TCryptFrame }

TCryptFrame = class(TWinControl)
private
  Crypt   :TCrypt;
  UserList   :TUserList;
  fFriendId :integer;
  fLoadUser   :boolean;
  fScroll   :TScrollBox;
  fPanel   :TPanel;
  fMemo   :TMemo;
  fMyFace   :TImage;
  fFriendFace   :TImage;
  fSendBtn   :TButton;
  fAtachBtn   :TButton;
  // Работа с файлами
  fFileOpen   :TOpenDialog;
  fFileList   :TStringList;
  fFileLabel   :TLabel;
  // Мои данные
  fUserName   :string;
  // Данные собеседника
  fFriendName   :string;
  // Список сообщений
  fMsg   :array of TMessages;
  function GetFriendName   :string;
  function GetUserName   :string;
  procedure SetUserName (AValue   :string);
  procedure SetFriendName (AValue   :string);
  procedure MemoKeyUp (Sender   :TObject; var Key   :word; Shift   :TShiftState);
  procedure SendThisMsg (Sender   :TObject);
  procedure AttachFiles (Sender   :TObject);
  // Добавить сообщение
  function Add (UserName, szText   :string; Date   :TDate; Avatar   :TPicture;
    Files   :TFiles)   :boolean;
public
  constructor Create (AOwner   :TComponent); override;
  destructor Destroy; override;
  // Количество сообщений
  function Count   :integer;
  // Аватарки
  procedure SetUserPic (FileName   :string);
  procedure SetFriendPic (FileName   :string);
  procedure Debug;
  // Сообщения
  procedure Send (szText   :string; Date   :TDateTime; Files   :TFiles);
  procedure Recv (szText   :string; Date   :TDateTime; Files   :TFiles);
  procedure AddFile ();
  function LoadUser(Email, Password, FindUser :string):boolean;
published
  property UserName   :string read GetUserName write SetUserName;
  property FriendName   :string read GetFriendName write SetFriendName;
  property FriendId :integer read fFriendId write fFriendId;
end;


implementation

{ TCryptFrame }

function TCryptFrame.GetFriendName   :string;
begin
  Result := fFriendName;
end;

function TCryptFrame.GetUserName   :string;
begin
  Result := fUserName;
end;

constructor TCryptFrame.Create (AOwner   :TComponent);
var
  i :integer;
  Mail, Path :string;
begin
  inherited Create (AOwner);
  Crypt           := TCrypt.Create;
  fLoadUser       := False;
  {
  Crypt.HostSmtp:= 'smtp.yandex.ru';
  if Crypt.Pop3Login ('pop.yandex.ru', 'CryptoChat.test@yandex.ru', '130492') then
  begin
    Mail := Crypt.GetMail (1);
    //ShowMessage ('Количество секций: ' + IntToStr (Crypt.GetCountSection (Mail)));
    for i := 0 to Crypt.GetCountSection (Crypt.GetMail (1)) - 1 do
    begin
      //ShowMessage ('Секция - полностью: ' + Crypt.GetSection (Mail, i));
      //ShowMessage ('Секция - заголовок: ' + Crypt.GetSectionHead (Mail, i));
      //ShowMessage ('Секция - тело: ' + Crypt.GetSectionBody (Mail, i));
      //ShowMessage ('Секция - имя вложения: ' + Crypt.GetSectionFileName (Mail, i));
      Path:= ExtractFilePath (Application.ExeName) + 'Mail/' + Crypt.GetSectionFileName (Mail, i);
      //ShowMessage(Path);
      if Crypt.GetSectionFileName (Mail, i) <> '' then
        Crypt.SaveSectionToFile (Path, Mail, i);
      Crypt.KeySend('everhest1@yandex.ru');
    end;
  end;}
  // Дополнительные компоненты
  fScroll         := TScrollBox.Create (self);
  fScroll.Parent  := self;
  // Панелька
  fPanel          := TPanel.Create (self);
  fPanel.Parent   := self;
  fPanel.Height   := 100;
  fPanel.Align    := alBottom;
  // Ввод текста
  fMemo           := TMemo.Create (fPanel);
  fMemo.Parent    := fPanel;
  fMemo.OnKeyUp   := @MemoKeyUp;
  // Моя фотка
  fMyFace         := TImage.Create (fPanel);
  fMyFace.Parent  := fPanel;
  // Фотка собеседника
  fFriendFace     := TImage.Create (fPanel);
  fFriendFace.Parent := fPanel;
  // Расположение
  fScroll.Align   := alClient;
  fScroll.AutoScroll := True;
  fMyFace.Top     := 4;
  fFriendFace.Top := 4;
  fMyFace.Left    := 4;
  // Положение фотки собеседника
  fFriendFace.Left := fPanel.Width - 70;
  fFriendFace.Width := 64;
  fFriendFace.Proportional := True;
  fFriendFace.Anchors := [akRight, akTop];
  fFriendFace.Stretch := True;
  // Моего лица
  fMyFace.Width   := 64;
  fMyFace.Stretch := True;
  fMyFace.Proportional := True;
  // Положение окна ввода текста
  fMemo.Left      := 74;
  fMemo.Top       := 4;
  fMemo.Height    := fPanel.Height - 38;
  fMemo.Width     := fPanel.Width - 152;
  fMemo.Anchors   := [akTop, akRight, akBottom, akLeft];
  // Кнопочки
  fSendBtn        := TButton.Create (fPanel);
  with fSendBtn do
  begin
    Parent  := fPanel;
    Top     := fPanel.Height - 32;
    Left    := 74;
    Width   := 94;
    Height  := 26;
    Caption := 'Отправить';
    OnClick := @SendThisMsg;
  end;
  fAtachBtn := TButton.Create (fPanel);
  with fAtachBtn do
  begin
    Parent  := fPanel;
    Top     := fPanel.Height - 32;
    Left    := 174;
    Width   := 94;
    Height  := 26;
    Caption := 'Прикрепить';
    OnClick := @AttachFiles;
  end;
  // Список файлов
  fFileList  := TStringList.Create;
  fFileLabel := TLabel.Create (fPanel);
  with fFileLabel do
  begin
    Parent     := fPanel;
    Caption    := 'Прикреплённые файлы (0)';
    Top        := fPanel.Height - 28;
    Left       := fMemo.Width + 74 - fFileLabel.Width;
    Height     := 26;
    font.Color := clNavy;
    Anchors    := [akRight, akTop];
  end;
  Align := alClient;
end;

destructor TCryptFrame.Destroy;
begin
  Crypt.Free;
  fScroll.FreeOnRelease;
  fPanel.FreeOnRelease;
  fMemo.FreeOnRelease;
  fMyFace.FreeOnRelease;
  fFriendFace.FreeOnRelease;
  fSendBtn.FreeOnRelease;
  fAtachBtn.FreeOnRelease;
  fFileList.Free;
  fFileLabel.FreeOnRelease;
  inherited Destroy;
end;

function TCryptFrame.Add (UserName, szText   :string; Date   :TDate;Avatar   :TPicture;
Files   :TFiles)   :boolean;
begin
  Result := True;
  // Выделяем память
  SetLength (fMsg, Length (fMsg) + 1);
  fmsg[High (fMsg)] := TMessages.Create (fScroll);
  fmsg[High (fMsg)].Parent := fScroll;
  // Выставляем позицию сообщения
  if High (fMsg) > 0 then
    fmsg[High (fMsg)].Top := fmsg[High (fMsg) - 1].Top + fmsg[High (fMsg) - 1].Height
  else
    fmsg[High (fMsg)].Top := 0;
  fmsg[High (fMsg)].Align := alTop;
  fmsg[High (fMsg)].ReAlign;
  // Содержимое диалога
  fMsg[High (fMsg)].Date := Date;
  fMsg[High (fMsg)].User := UserName;
  fMsg[High (fMsg)].Text := szText;
  fMsg[High (fMsg)].LoadImage (Avatar);
  fScroll.VertScrollBar.Position := fScroll.VertScrollBar.Range; // ? Ошибка ?
end;

procedure TCryptFrame.SetUserName (AValue   :string);
begin
  fUserName := AValue;
end;

procedure TCryptFrame.SetFriendName (AValue   :string);
begin
  fFriendName := AValue;
end;

procedure TCryptFrame.MemoKeyUp (Sender   :TObject;
var Key   :word; Shift   :TShiftState);
const
  VK_RETURN = 13;
begin
  if (ssCtrl in Shift) and (Key = VK_RETURN) then
    SendThisMsg (Sender);
end;

procedure TCryptFrame.SendThisMsg (Sender   :TObject);
begin
  Send (fMemo.Text, now, nil);
  fFileList.Clear;
  fMemo.Clear;
end;

procedure TCryptFrame.AttachFiles (Sender   :TObject);
begin
  AddFile;
end;

function TCryptFrame.Count   :integer;
begin
  Result := High (fMsg) + 1;
end;

procedure TCryptFrame.SetUserPic (FileName   :string);
begin
  fMyFace.Picture.LoadFromFile (FileName);
end;

procedure TCryptFrame.SetFriendPic (FileName   :string);
begin
  fFriendFace.Picture.LoadFromFile (FileName);
end;

procedure TCryptFrame.Debug;
begin
  ShowMessageFmt ('Range: %d \n Position: %d',
    [fScroll.VertScrollBar.Range, fScroll.VertScrollBar.Position]);
end;

procedure TCryptFrame.Send (szText   :string; Date   :TDateTime; Files   :TFiles);
begin
  Add (UserName, szText, Date, fMyFace.Picture, Files);
  fMsg[High (fMsg)].ReAlign;
end;

procedure TCryptFrame.Recv (szText   :string; Date   :TDateTime; Files   :TFiles);
begin
  Add (FriendName, szText, Date, fFriendFace.Picture, Files);
  fMsg[High (fMsg)].ReAlign;
end;

procedure TCryptFrame.AddFile;
begin
  fFileOpen := TOpenDialog.Create (self);
  fFileOpen.Options := [ofAllowMultiSelect];
  if fFileOpen.Execute then
    fFileList.AddStrings (fFileOpen.Files);
  fFileLabel.Caption := 'Прикреплённые файлы (' + IntToStr (fFileList.Count) + ')';
  fFileOpen.Free;
end;

function TCryptFrame.LoadUser(Email, Password, FindUser :string):boolean;
var
  i :integer;
begin
  // Поиск пользователя по введённым данным
  if not fLoadUser then
  begin
    UserList := TUserList.Create (Self);
    if not UserList.LoginUser(EMail, Password) then
    begin
      raise Exception.Create('Проблема аутентификации в TCryptFrame.LoadUser');
      Exit;
    end
    else
    begin
      fLoadUser:= True;
      for i:= 0 to UserList.GetFriendCount do
        if FindUser = (UserList.GetFriend(i).NickName + ' < ' +
          UserList.GetFriend(i).Email + ' >') then
        begin
          friendId:= i;
          ShowMessage(UserList.GetFriend(i).Email);
        end;
    end;
  end;
  Result := fLoadUser;
end;

end.
