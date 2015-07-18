unit FrameLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, Engine.DataBases;

type

  { TFrameLogin }

  TFrameLogin = class(TFrame)
    ButtonRegistration: TButton;
    ButtonLogin: TButton;
    EditEmail: TLabeledEdit;
    EditPassword: TLabeledEdit;
    LabelInfoText: TLabel;
    LabelUps: TLabel;
    Panel: TPanel;
    procedure ButtonLoginClick(Sender: TObject);
    procedure ButtonRegistrationClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

uses Main, FrameRegisterUser;

{ TFrameLogin }

procedure TFrameLogin.FrameResize(Sender: TObject);
begin
  Panel.Left := TFrame(Sender).Width div 2 - Panel.Width div 2;
  Panel.Top := TFrame(Sender).Height div 2 - Panel.Height div 4;
end;

procedure TFrameLogin.ButtonLoginClick(Sender: TObject);
// Вход в систему
begin
  if DataBase.Login(EditEmail.Text, EditPassword.Text) then
    MainForm.ShowFrameDialogs
  else
    MessageDlg('Ошибка', 'Ошибка входа в систему, вероятно не верно введены данные...', mtWarning, [mbOK], 0);
end;

procedure TFrameLogin.ButtonRegistrationClick(Sender: TObject);
// Регистрация нового пользователя
begin
  MainForm.ShowRegisterFrame;
  MainForm.FrameRegister.NotFirstRegistration(FRAME_LOGIN);
end;

end.

