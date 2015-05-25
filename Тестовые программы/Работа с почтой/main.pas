unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Engine.Mail;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    EditHostOut: TLabeledEdit;
    EditPortOut: TLabeledEdit;
    EditUserName: TLabeledEdit;
    EditUserPassword: TLabeledEdit;
    EditHostIn: TLabeledEdit;
    EditPortIn: TLabeledEdit;
    MainMenu: TMainMenu;
    Memo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure SetLogLine(AValue: String);
    { private declarations }
  public
    { public declarations }
    mail: TCustomMail;
    property Log: String write SetLogLine;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  mail:= TCustomMail.Create;
  mail.Connected:= true;
  Log:= BoolToStr(mail.Connected, 'Connected', 'Error');
end;

procedure TMainForm.SetLogLine(AValue: String);
begin
  Memo.Append(AValue);
end;

end.

