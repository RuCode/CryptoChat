unit NewUser;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TNewUserForm }

  TNewUserForm = class(TForm)
    Bevel1: TBevel;
    SaveBtn: TButton;
    CanfcelBtn: TButton;
    CheckBox: TCheckBox;
    EditEmail: TLabeledEdit;
    EditEmailPWD: TLabeledEdit;
    EditUserPwd: TLabeledEdit;
    EditName: TLabeledEdit;
    EditRePwd: TLabeledEdit;
    procedure CheckBoxChange(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  NewUserForm: TNewUserForm;

implementation

{$R *.lfm}

{ TNewUserForm }

procedure TNewUserForm.CheckBoxChange(Sender: TObject);
begin
  if not CheckBox.Checked then
  begin
    EditUserPwd.PasswordChar:= '*';
    EditEmailPWD.PasswordChar:= '*';
    EditRePwd.PasswordChar:= '*';
  end else begin
    EditUserPwd.PasswordChar:= #0;
    EditEmailPWD.PasswordChar:= #0;
    EditRePwd.PasswordChar:= #0;
  end;
end;

procedure TNewUserForm.EditNameChange(Sender: TObject);
begin

end;

end.

