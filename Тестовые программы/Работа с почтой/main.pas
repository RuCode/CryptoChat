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
    EditPortOut: TLabeledEdit;
    EditUserName: TLabeledEdit;
    EditUserPassword: TLabeledEdit;
    EditHostIn: TLabeledEdit;
    EditPortIn: TLabeledEdit;
    MainMenu: TMainMenu;
    Memo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure SetLogLine(AValue: string);
    { private declarations }
  public
    { public declarations }
    mail: TCustomMail;
    property Log: string write SetLogLine;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
var
  list: TStringList;
begin

  mail := TCustomMail.Create;
  mail.Connected := True;
  Log := BoolToStr(mail.Connected, 'Connected', 'Error');
  Log := mail.FullResult;

  list:= TStringList.Create;
  mail.GetFolderList(list);
  Memo.Lines.Assign(list);
  list.Free;
end;

procedure TMainForm.SetLogLine(AValue: string);
begin
  Memo.Append(AValue);
end;

end.

