unit Main;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
Dialogs, Menus, ComCtrls, ExtCtrls, StdCtrls,
// My forms
NewUser, sqlite3conn,
// My controls
CryptFrame
, types;

type

{ TMainForm }

TMainForm = class(TForm)
  CheckBoxSaveMee :TCheckBox;
  ContactTabSheet :TTabSheet;
  ListBox1      :TListBox;
  MenuItem10: TMenuItem;
  MenuItem8     :TMenuItem;
  MenuItem9     :TMenuItem;
  PageControl1  :TPageControl;
  TabSheet2     :TTabSheet;
  UserNameComboBox :TComboBox;
  CreateUserBtn :TButton;
  LoginBtn      :TButton;
  UserPwdEdit   :TEdit;
  LabelMsgLogin :TLabel;
  LabelWarining :TLabel;
  LabelMsg      :TLabel;
  LabelWariningLogin :TLabel;
  MainMenu      :TMainMenu;
  MenuFile      :TMenuItem;
  MenuItem1     :TMenuItem;
  MenuItem2     :TMenuItem;
  MenuItem3     :TMenuItem;
  MenuItem4     :TMenuItem;
  MenuItem5     :TMenuItem;
  MenuItem6     :TMenuItem;
  MenuItem7     :TMenuItem;
  MenuUsers     :TMenuItem;
  PageControl   :TPageControl;
  RegisterTabSheet :TTabSheet;
  SQLite3Connection :TSQLite3Connection;
  LoginTabSheet :TTabSheet;
  procedure CreateUserBtnClick (Sender :TObject);
  procedure FormCreate (Sender :TObject);
  procedure LoginTabSheetResize (Sender :TObject);
  procedure MenuItem10Click(Sender: TObject);
  procedure MenuItem9Click (Sender :TObject);
  procedure RegisterTabSheetResize (Sender :TObject);
private
  { private declarations }
public
  { public declarations }
end;

var
MainForm :TMainForm;
cf :TCryptFrame;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.RegisterTabSheetResize (Sender :TObject);
begin
  with LabelWarining do
    Left := Round (RegisterTabSheet.Width / 2) - Round (Width / 2);
  with CreateUserBtn do
    Left := Round (RegisterTabSheet.Width / 2) - Round (Width / 2);
  with LabelMsg do
    Left := Round (RegisterTabSheet.Width / 2) - Round (Width / 2);
  LabelWarining.Top := Round (RegisterTabSheet.Height / 2) - 60;
  LabelMsg.Top := LabelWarining.Top + 35;
  CreateUserBtn.Top := LabelWarining.Top + 70;
end;

procedure TMainForm.CreateUserBtnClick (Sender :TObject);
begin
  NewUserForm.ShowModal;
end;

procedure TMainForm.FormCreate (Sender :TObject);
begin
  cf := TCryptFrame.Create (TabSheet2);
  application.ProcessMessages;
  cf.Parent := TabSheet2;
  cf.Align  := alClient;
  cf.SetUserPic ('/home/anton/Изображения/ava.jpg');
  cf.UserName:= 'Антон';
  cf.FriendName:= 'Анна';
  cf.SetFriendPic ('/home/anton/Изображения/ava2.jpg');
end;

procedure TMainForm.LoginTabSheetResize (Sender :TObject);
begin
  with LabelWariningLogin do
    Left := Round (LoginTabSheet.Width / 2) - Round (Width / 2);
  with LabelMsgLogin do
    Left := Round (LoginTabSheet.Width / 2) - Round (Width / 2);
  with UserNameComboBox do
    Left := Round (LoginTabSheet.Width / 2) - Round (Width / 2);
  with UserPwdEdit do
    Left := Round (LoginTabSheet.Width / 2) - Round (Width / 2);
  CheckBoxSaveMee.Left := UserPwdEdit.Left;
  LoginBtn.Left := UserPwdEdit.Left + UserPwdEdit.Width - LoginBtn.Width;

  with LabelWariningLogin do
  begin
    Top := Round (LoginTabSheet.Height / 2) - 110;
    LabelMsgLogin.Top := Top + 35;
    UserNameComboBox.Top := Top + 70;
    UserPwdEdit.Top := Top + 105;
    CheckBoxSaveMee.Top := Top + 144;
    LoginBtn.Top := Top + 140;
  end;
end;

procedure TMainForm.MenuItem10Click(Sender: TObject);
begin
  cf.Recv('Привет))', now, nil);
end;

procedure TMainForm.MenuItem9Click (Sender :TObject);
begin
  cf.Debug;
end;

end.

