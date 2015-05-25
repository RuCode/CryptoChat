unit NewUser;

{$mode objfpc}

interface

uses
Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
StdCtrls, ExtDlgs;

type
TRegFunc = procedure (Nick, FirstName, LastName, Email, Password, ImgPath   :string);

type

{ TNewUserForm }

TNewUserForm = class(TForm)
  Bevel1       :TBevel;
  Bevel2       :TBevel;
  OpenDialog: TOpenPictureDialog;
  SelAvaBtn      :TButton;
  CancelBtn    :TButton;
  EditEmailPWD: TLabeledEdit;
  Image        :TImage;
  SaveBtn      :TButton;
  CanfcelBtn   :TButton;
  CheckBox     :TCheckBox;
  EditEmail    :TLabeledEdit;
  EditNickName   :TLabeledEdit;
  procedure SelAvaBtnClick(Sender: TObject);
  procedure CancelBtnClick(Sender: TObject);
  procedure CheckBoxChange (Sender   :TObject);
  procedure SaveBtnClick (Sender   :TObject);
private
  { private declarations }
  procedure StoreValues;
public
  { public declarations }
  NickName, Email, Password, PathToAvatar: String;
  procedure SetCaptionsWithUsers();
  procedure SetCaptionsWithContacts();
end;

var
NewUserForm :TNewUserForm;

implementation

{$R *.lfm}

{ TNewUserForm }

procedure TNewUserForm.CheckBoxChange (Sender   :TObject);
begin
  if not CheckBox.Checked then
    EditEmailPWD.PasswordChar  := '*'
  else
    EditEmailPWD.PasswordChar  := #0;
end;

procedure TNewUserForm.CancelBtnClick(Sender: TObject);
begin
  CancelBtn.ModalResult:= mrCancel;
end;

procedure TNewUserForm.SelAvaBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    PathToAvatar:= OpenDialog.FileName;
    Image.Picture.LoadFromFile(PathToAvatar);
  end;
end;

procedure TNewUserForm.SaveBtnClick (Sender   :TObject);
begin
  StoreValues;
  SaveBtn.ModalResult:= mrOk;
end;

procedure TNewUserForm.StoreValues;
begin
   NickName:= trim(EditNickName.Text);
   Email:= trim(EditEmail.Text);
   Password:= trim(EditEmailPWD.Text);
end;

procedure TNewUserForm.SetCaptionsWithUsers;
begin
  Caption:= 'Создание нового пользователя';
  EditNickName.EditLabel.Caption:= 'Ваше имя или псевдоним: ';
  EditEmail.EditLabel.Caption:= 'Ваш адрес EMail:';
  EditEmailPWD.Visible:= True;
  CheckBox.Visible:= True;
end;

procedure TNewUserForm.SetCaptionsWithContacts;
begin
  Caption:= 'Добавление нового собеседника';
  EditNickName.EditLabel.Caption:= 'Псевдоним: ';
  EditEmail.EditLabel.Caption:= 'EMail:';
  EditEmailPWD.Visible:= False;
  CheckBox.Visible:= False;
end;

end.
