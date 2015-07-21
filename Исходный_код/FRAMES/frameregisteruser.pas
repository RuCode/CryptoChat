unit FrameRegisterUser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ExtDlgs, Engine.DataBases, Dialogs;

const
  FRAME_LOGIN = 1;

type

  { TFrameRegisterUser }

  TFrameRegisterUser = class(TFrame)
    Bevel: TBevel;
    ButtonRegister: TButton;
    ButtonAvatar: TButton;
    ButtonCancel: TButton;
    CheckBox: TCheckBox;
    EditName: TLabeledEdit;
    EditMail: TLabeledEdit;
    EditPassword: TLabeledEdit;
    EditPasswordEmail: TLabeledEdit;
    EditHostIncoming: TLabeledEdit;
    EditHostOutgoing: TLabeledEdit;
    EditPortImap: TLabeledEdit;
    EditPortSmtp: TLabeledEdit;
    Image: TImage;
    LabelAvatar: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    Panel: TPanel;
    procedure ButtonAvatarClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonRegisterClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
    ToBackPage: integer; // Индекс предыдущей страницы
  public
    procedure FirstRegistration;
    procedure NotFirstRegistration(BackPage: integer = 0);
  end;

implementation

{$R *.lfm}

uses Main;

{ TFrameRegisterUser }

procedure TFrameRegisterUser.FrameResize(Sender: TObject);
begin
  Panel.Left := TFrame(Sender).Width div 2 - Panel.Width div 2;
  Panel.Top := TFrame(Sender).Height div 2 - Panel.Height div 2 - 30;
end;

procedure TFrameRegisterUser.FirstRegistration;
// При первой реге убираем кнопку "отмена"
begin
  ButtonCancel.Visible := False;
  ButtonRegister.Left := TFrame(self).Width - ButtonRegister.Width - ButtonAvatar.Left;
end;

procedure TFrameRegisterUser.NotFirstRegistration(BackPage: integer);
// При последующих регах ставим кнопку "отмена"
begin
  ButtonCancel.Visible := True;
  ButtonCancel.Left := TFrame(self).Width - ButtonCancel.Width - ButtonAvatar.Left;
  ButtonRegister.Left := ButtonCancel.Left - ButtonRegister.Width - 4;
  ToBackPage := BackPage;
end;

procedure TFrameRegisterUser.ButtonAvatarClick(Sender: TObject);
// Выбор аватарки
begin
  if OpenPictureDialog.Execute then
    Image.Picture.LoadFromFile(OpenPictureDialog.FileName);
end;

procedure TFrameRegisterUser.ButtonCancelClick(Sender: TObject);
// Возврат на предыдущую страницу
begin
  case ToBackPage of
    FRAME_LOGIN: MainForm.ShowLoginFrame;
  end;
end;

procedure TFrameRegisterUser.ButtonRegisterClick(Sender: TObject);
// Регистрация нового пользователя
begin
  // Проверка полей
  if not DataBase.AddUser(EditName.Text, EditPassword.Text, EditMail.Text, OpenPictureDialog.FileName) then
    MessageDlg('Ошибка', 'Ошибка 1: Возникли проблемы с созданием нового пользователя...', TMSgDlgType.mtError, [mbOK], 0)
  else
  // Пользователь создан требуется войти в систему
  if DataBase.Login(EditMail.Text, EditPassword.Text) then
    MainForm.ShowFrameDialogs;
end;

end.


