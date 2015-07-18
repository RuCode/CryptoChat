unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, FrameLogin;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FrameLogin: TFrameLogin;
    procedure ShowLoginFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ShowLoginFrame;
end;

procedure TMainForm.ShowLoginFrame;
// Показать фрэйм входа в систему
begin
  if not Assigned(FrameLogin) then
  begin
    FrameLogin := TFrameLogin.Create(MainForm);
    FrameLogin.Align := alClient;
  end;
  FrameLogin.Parent := MainForm;
end;

end.

