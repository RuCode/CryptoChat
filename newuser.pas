unit NewUser;

{$mode objfpc}

interface

uses
Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
StdCtrls;

type
TRegFunc = procedure (Nick, FirstName, LastName, Email, Password, ImgPath   :string);

type

{ TNewUserForm }

TNewUserForm = class(TForm)
  Bevel1       :TBevel;
  Bevel2       :TBevel;
  Button1      :TButton;
  CancelBtn    :TButton;
  EditEmailPWD: TLabeledEdit;
  Image        :TImage;
  SaveBtn      :TButton;
  CanfcelBtn   :TButton;
  CheckBox     :TCheckBox;
  EditEmail    :TLabeledEdit;
  EditFirstName   :TLabeledEdit;
  EditNickName   :TLabeledEdit;
  EditLastName   :TLabeledEdit;
  procedure CancelBtnClick(Sender: TObject);
  procedure CheckBoxChange (Sender   :TObject);
  procedure SaveBtnClick (Sender   :TObject);
private
  { private declarations }
  procedure StoreValues;
public
  { public declarations }
  NickName, FirstName, LastName, Email, Password, PathToAvatar: String;
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

procedure TNewUserForm.SaveBtnClick (Sender   :TObject);
begin
  StoreValues;
  SaveBtn.ModalResult:= mrOk;
end;

procedure TNewUserForm.StoreValues;
begin
   NickName:= EditNickName.Text;
   FirstName:= EditFirstName.Text;
   LastName:= EditLastName.Text;
   Email:= EditEmail.Text;
   Password:= EditEmailPWD.Text;
end;

procedure TNewUserForm.SetCaptionsWithUsers;
begin
  Caption:= 'Создание нового пользователя';
  EditNickName.EditLabel.Caption:= 'Ваше имя или псевдоним: ';
  EditFirstName.EditLabel.Caption:= 'Ваше имя: ';
  EditLastName.EditLabel.Caption:= 'Ваша фамилия:';
  EditEmail.EditLabel.Caption:= 'Ваш адрес EMail:';
  EditEmailPWD.Visible:= True;
end;

procedure TNewUserForm.SetCaptionsWithContacts;
begin
  Caption:= 'Добавление нового собеседника';
  EditNickName.EditLabel.Caption:= 'Псевдоним: ';
  EditFirstName.EditLabel.Caption:= 'Имя: ';
  EditLastName.EditLabel.Caption:= 'Фамилия:';
  EditEmail.EditLabel.Caption:= 'EMail:';
  EditEmailPWD.Visible:= False;
end;

end.
