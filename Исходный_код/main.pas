unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, FrameLogin, FrameOfferRegisterUser, FrameRegisterUser;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FrameLogin: TFrameLogin;
    FrameOfferRegister: TFrameOfferRegisterUser;
    FrameRegister: TFrameRegisterUser;
    {: Показать фрэйм предложения регистрации пользователя}
    procedure ShowOfferRegisterFrame;
    {: Показать фрэйм регистрации пользователя}
    procedure ShowRegisterFrame;
    {: Показать фрэйм входа пользователя}
    procedure ShowLoginFrame;
    {: Скрыть все фрэймы}
    procedure HideAllFrames;
    {: Установить заголовок основного окна}
    procedure SetCaptionWithForm(Frame: TFrame);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //  ShowLoginFrame;
  ShowOfferRegisterFrame;
end;

procedure TMainForm.ShowOfferRegisterFrame;
// Показать фрэйм регистрации пользователя
begin
  HideAllFrames;
  if not Assigned(FrameOfferRegister) then
  begin
    FrameOfferRegister := TFrameOfferRegisterUser.Create(MainForm);
    FrameOfferRegister.Align := alClient;
  end;
  FrameOfferRegister.Parent := MainForm;
  FrameOfferRegister.Visible := True;
  SetCaptionWithForm(FrameOfferRegister);
end;

procedure TMainForm.ShowRegisterFrame;
// Показать фрэйм регистрации пользователя
begin
  HideAllFrames;
  if not Assigned(FrameRegister) then
  begin
    FrameRegister := TFrameRegisterUser.Create(MainForm);
    FrameRegister.Align := alClient;
  end;
  FrameRegister.Parent := MainForm;
  FrameRegister.Visible := True;
  SetCaptionWithForm(FrameRegister);
end;

procedure TMainForm.ShowLoginFrame;
// Показать фрэйм входа в систему
begin
  HideAllFrames;
  if not Assigned(FrameLogin) then
  begin
    FrameLogin := TFrameLogin.Create(MainForm);
    FrameLogin.Align := alClient;
  end;
  FrameLogin.Parent := MainForm;
  FrameLogin.Visible := True;
  SetCaptionWithForm(FrameLogin);
end;

procedure TMainForm.HideAllFrames;
// Скрыть все фрэймы
begin
  if Assigned(FrameOfferRegister) then
    FrameOfferRegister.Visible := False;
  if Assigned(FrameRegister) then
    FrameRegister.Visible := False;
  if Assigned(FrameLogin) then
    FrameLogin.Visible := False;
end;

procedure TMainForm.SetCaptionWithForm(Frame: TFrame);
// Установить заголовок основного окна
begin
  Caption:= 'CryptoChat: ' + Frame.Hint;
end;

end.

