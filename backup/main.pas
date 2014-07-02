unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, FileUtil, Forms, Graphics,
  Menus, StdCtrls, SysUtils,
  test, NewUser, CryptChat, MainCrypt, WidgetChats;

type

  { TMainForm }

  TMainForm = class(TForm)
    GoRegBtn:      TButton;
    CheckBoxSaveMee: TCheckBox;
    ContactTabSheet: TTabSheet;
    PopUpContact:  TMenuItem;
    MenuItem10:    TMenuItem;
    MenuItem8:     TMenuItem;
    MenuItem9:     TMenuItem;
    ContactListPopup: TPopupMenu;
    UserNameComboBox: TComboBox;
    CreateUserBtn: TButton;
    LoginBtn:      TButton;
    UserPwdEdit:   TEdit;
    LabelMsgLogin: TLabel;
    LabelWarining: TLabel;
    LabelMsg:      TLabel;
    LabelWariningLogin: TLabel;
    MainMenu:      TMainMenu;
    MenuFile:      TMenuItem;
    MenuNewContact: TMenuItem;
    MenuItem2:     TMenuItem;
    MenuItem3:     TMenuItem;
    MenuItem4:     TMenuItem;
    MenuItem5:     TMenuItem;
    MenuItem6:     TMenuItem;
    MenuItem7:     TMenuItem;
    MenuUsers:     TMenuItem;
    PageControl:   TPageControl;
    RegisterTabSheet: TTabSheet;
    LoginTabSheet: TTabSheet;
    procedure CreateUserBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoginBtnClick(Sender: TObject);
    procedure LoginTabSheetResize(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure MenuNewContactClick(Sender: TObject);
    procedure RegisterTabSheetResize(Sender: TObject);
    procedure UserPwdEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
    APP_PATH: string;
    fChat:    TCryptChat;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.LoginTabSheetResize(Sender: TObject);
begin
  with LabelWariningLogin do
    Left := Round(LoginTabSheet.Width / 2) - Round(Width / 2);
  with LabelMsgLogin do
    Left := Round(LoginTabSheet.Width / 2) - Round(Width / 2);
  with UserNameComboBox do
    Left := Round(LoginTabSheet.Width / 2) - Round(Width / 2);
  with UserPwdEdit do
    Left := Round(LoginTabSheet.Width / 2) - Round(Width / 2);
  with GoRegBtn do
    Left := Round(LoginTabSheet.Width / 2) - Round(Width / 2);
  CheckBoxSaveMee.Left := UserPwdEdit.Left;
  LoginBtn.Left := UserPwdEdit.Left + UserPwdEdit.Width - LoginBtn.Width;

  with LabelWariningLogin do
  begin
    Top := Round(LoginTabSheet.Height / 2) - 110;
    LabelMsgLogin.Top := Top + 35;
    UserNameComboBox.Top := Top + 70;
    UserPwdEdit.Top := Top + 105;
    CheckBoxSaveMee.Top := Top + 144;
    LoginBtn.Top := Top + 140;
    GoRegBtn.Top := Top + 180;
  end;
end;

procedure TMainForm.MenuItem10Click(Sender: TObject);
begin
  Randomize;
  fChat.Recv(IntToStr(Random(100000)), Now, nil);
end;

procedure TMainForm.MenuItem9Click(Sender: TObject);
begin
  TestForm:= TTestForm.Create(self);
  TestForm.ShowModal;
  TestForm.Free;
end;

procedure TMainForm.MenuNewContactClick(Sender: TObject);
var
  NewUserForm: TNewUserForm;
  AFriend: TFriendEntry;
begin
  if not fCHat.isUserLogin then exit;
  NewUserForm := TNewUserForm.Create(MainForm);
  NewUserForm.SetCaptionsWithContacts;
  with NewUserForm do
  begin
    if ShowModal = mrOk then
    begin
       AFriend.NickName := NewUserForm.NickName;
       if AFriend.NickName = '' then exit;
       AFriend.Email    := NewUserForm.Email;
       if AFriend.EMail = '' then exit;
       if fChat.AddFriend(AFriend) then
       begin
          // Обновить список контактов (TListBox)
          fCHat.GlobalUpdate;
          // Отправить пакет на авторизацию
          // Обазначить как не авторизованный и проверять входящие письма с подтверждением авторизации
       end
       else raise Exception.Create('Не возможно добавить нового друга с Email: ' + AFriend.Email);
    end;
  end;
  NewUserForm.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  APP_PATH := ExtractFilePath(Application.ExeName);
  fChat    := TCryptChat.Create(ContactTabSheet);
  with fChat do
  begin
    Left   := 0;
    Width  := 400;
    Top    := 0;
    Height := 400;
    Align  := alClient;
    Parent := ContactTabSheet;
  end;
  if not fChat.OpenDB(APP_PATH + 'DataBase.db3') then
    raise Exception.Create('Не могу открыть базу данных: ' + APP_PATH + 'DataBase.db3')
  else begin
    for i := 0 to fChat.GetCountUsers - 1 do
        // Вот тут надо получить все email пользователей
        UserNameComboBox.Items.Add(fChat.Users[i].Email);
    if UserNameComboBox.Items.Count = 0 then
        PageControl.ActivePage:= RegisterTabSheet
    else
        PageControl.ActivePage:= LoginTabSheet;
  end;
end;

procedure TMainForm.LoginBtnClick(Sender: TObject);
begin
  if not fChat.LoginUser(UserNameComboBox.Text, UserPwdEdit.Text) then
    raise Exception.Create('Не могу пройти авторизацию, вероятно не верно введены данные...')
  else
  begin
    // Тут инициализация контактов
    fChat.GlobalUpdate;
    PageControl.ActivePage := ContactTabSheet;
  end;
end;

procedure TMainForm.CreateUserBtnClick(Sender: TObject);
var
  NewUserForm: TNewUserForm;
  AUser: TUserEntry;
begin
  NewUserForm := TNewUserForm.Create(MainForm);
  with NewUserForm do
  begin
    if ShowModal = mrOk then
    begin
      AUser.Email     := Email;
      AUser.NickName  := NickName;
      AUser.HashPwd   := Password;
      if not fChat.AddUser(AUser) then
        raise Exception.Create('Не могу создать пользователя: ' + NickName)
      else
      if not fChat.LoginUser(Email, Password) then
        raise Exception.Create('Не могу создать пользователя: ' + NickName)
      else
      begin
        if PathToAvatar <> '' then
           fChat.SetUserImage(fChat.GetCountUsers-1, PathToAvatar);
        // Тут инициализация контактов
        fChat.GlobalUpdate;
        PageControl.ActivePage := ContactTabSheet;
      end;
    end;
  end;
  NewUserForm.Free;
end;

procedure TMainForm.RegisterTabSheetResize(Sender: TObject);
begin
  with LabelWarining do
    Left := Round(RegisterTabSheet.Width / 2) - Round(Width / 2);
  with CreateUserBtn do
    Left := Round(RegisterTabSheet.Width / 2) - Round(Width / 2);
  with LabelMsg do
    Left := Round(RegisterTabSheet.Width / 2) - Round(Width / 2);
  LabelWarining.Top := Round(RegisterTabSheet.Height / 2) - 60;
  LabelMsg.Top      := LabelWarining.Top + 35;
  CreateUserBtn.Top := LabelWarining.Top + 70;
end;

procedure TMainForm.UserPwdEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  VK_RETURN = 13;
begin
  if Key = VK_RETURN then
     LoginBtnClick(Self);
end;


end.
