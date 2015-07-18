unit FrameOfferRegisterUser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type
  { TFrameOfferRegisterUser }

  TFrameOfferRegisterUser = class(TFrame)
    ButtonRegister: TButton;
    LabelInfoText: TLabel;
    LabelUps: TLabel;
    procedure ButtonRegisterClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
  public
  end;

implementation

{$R *.lfm}

uses Main;

{ TFrameOfferRegisterUser }

procedure TFrameOfferRegisterUser.FrameResize(Sender: TObject);
begin
  ButtonRegister.Left := TFrame(Sender).Width div 2 - 90 {1/2 width btn};
  ButtonRegister.Width := 180;
  ButtonRegister.Top := TFrame(Sender).Height div 2 + ButtonRegister.Height;
end;

procedure TFrameOfferRegisterUser.ButtonRegisterClick(Sender: TObject);
// Открываем фрэйм ввода данных
begin
  MainForm.ShowRegisterFrame;
  MainForm.FrameRegister.FirstRegistration;
end;

end.

