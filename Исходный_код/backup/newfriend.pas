unit NewFriend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TNewFriendForm }

  TNewFriendForm = class(TForm)
    Bevel: TBevel;
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Image: TImage;
    EditNick: TLabeledEdit;
    EditName: TLabeledEdit;
    EditFamily: TLabeledEdit;
    EditEMail: TLabeledEdit;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  NewFriendForm: TNewFriendForm;

implementation

{$R *.lfm}

{ TNewFriendForm }

procedure TNewFriendForm.Button2Click(Sender: TObject);
begin
  SaveBtn.ModalResult:= mrOk;
end;

procedure TNewFriendForm.Button3Click(Sender: TObject);
begin
  SaveBtn.ModalResult:= mrCancel;
end;

end.

