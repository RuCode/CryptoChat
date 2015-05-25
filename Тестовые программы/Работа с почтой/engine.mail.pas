unit Engine.Mail;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, ExtCtrls, Forms, Controls, mimemess, mimepart, pop3send, smtpsend,
  synacode;

type

  { TCustomMail }

  TCustomMail = class(TObject)
  private
    fConnected: boolean;
    fLogin: string;
    fPassword: string;
    fSMTPHost: string;
    fSMTPPort: integer;
    fPOP3Host: string;
    fPOP3Port: integer;
    fPOP3Send: TPOP3Send;
    fMailMessage: TMimeMess;
    procedure SetConnected(AValue: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // Свойства которые должны быть заполнены для работы с почтой
    property Login: string read fLogin write flogin;
    property Password: string write fPassword;
    property SMTPHost: string read fSMTPHost write fSMTPHost;
    property SMTPPort: integer read fSMTPPort write fSMTPPort;
    property POP3Host: string read fPOP3Host write fPOP3Host;
    property POP3Port: integer read fPOP3Port write fPOP3Port;

    property Connected: boolean read fConnected write SetConnected;
  end;

implementation

{ TCustomMail }

procedure TCustomMail.SetConnected(AValue: boolean);
begin
  if fConnected = AValue then
    Exit;
  fPOP3Send.TargetHost := POP3Host;
  fPOP3Send.UserName := Login;
  fPOP3Send.Password := fPassword;
  fConnected := fPOP3Send.Login;
end;

constructor TCustomMail.Create;
begin
  inherited Create;
  fConnected := False;
  fLogin := 'i.rcode@yandex.ru';
  fPassword := '3jD253MxhE';
  fSMTPHost := 'smtp.yandex.ru';
  fSMTPPort := 465;
  fPOP3Host := 'pop.yandex.ru';
  fPOP3Port := 995;
end;

destructor TCustomMail.Destroy;
begin
  inherited Destroy;
end;

end.

