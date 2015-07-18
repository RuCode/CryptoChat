unit FrameRegisterUser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ExtDlgs;

type

  { TFrameRegisterUser }

  TFrameRegisterUser = class(TFrame)
    Bevel: TBevel;
    ButtonRegister: TButton;
    ButtonAvatar: TButton;
    ButtonCancel: TButton;
    EditName: TLabeledEdit;
    EditMail: TLabeledEdit;
    EditPassword: TLabeledEdit;
    Image: TImage;
    LabelAvatar: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    Panel: TPanel;
    procedure ButtonAvatarClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
  public
  end;

implementation

{$R *.lfm}

{ TFrameRegisterUser }

procedure TFrameRegisterUser.FrameResize(Sender: TObject);
begin
  Panel.Left := TFrame(Sender).Width div 2 - Panel.Width div 2;
  Panel.Top := TFrame(Sender).Height div 2 - Panel.Height div 2 - 40;
end;

procedure TFrameRegisterUser.ButtonAvatarClick(Sender: TObject);
// Выбор аватарки
begin
  if OpenPictureDialog.Execute then
    Image.Picture.LoadFromFile(OpenPictureDialog.FileName);
end;

end.

