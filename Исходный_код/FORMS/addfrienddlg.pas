unit AddFriendDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ExtDlgs;

type

  { TFormAddFriend }

  TFormAddFriend = class(TForm)
    Bevel: TBevel;
    ButtonAvatar: TButton;
    ButtonCancel: TButton;
    ButtonRegister: TButton;
    EditMail: TLabeledEdit;
    EditName: TLabeledEdit;
    Image: TImage;
    LabelAvatar: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    Panel: TPanel;
    procedure ButtonAvatarClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure Clear;
  end;

var
  FormAddFriend: TFormAddFriend;

implementation

{$R *.lfm}

{ TFormAddFriend }

procedure TFormAddFriend.ButtonAvatarClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    Image.Picture.LoadFromFile(OpenPictureDialog.FileName);
end;

procedure TFormAddFriend.Clear;
begin
  EditMail.Clear;
  EditName.Clear;
  Image.Picture.Clear;
  OpenPictureDialog.FileName := '';
end;

end.

