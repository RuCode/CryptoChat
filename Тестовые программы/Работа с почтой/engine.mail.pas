unit Engine.Mail;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, ExtCtrls, Forms, Controls, imapsend, ssl_openssl;

type

  { TCustomMail }

  TCustomMail = class(TObject)
  private
    fConnected: boolean;
    fLogin: string;
    fPassword: string;
    fIMAPHost: string;
    fIMAPPort: integer;
    fIMAPClient: TIMAPSend;
    function GetFullResult: string;
    procedure SetConnected(AValue: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure GetFolderList(var ListFolders: TStringList);
  public
    property Login: string read fLogin write flogin; // переименовать
    property Password: string write fPassword;
    property IMAPHost: string read fIMAPHost write fIMAPHost;
    property IMAPPort: integer read fIMAPPort write fIMAPPort;
    property Connected: boolean read fConnected write SetConnected;
    property FullResult: string read GetFullResult;// переименовать
  end;

implementation

{ TCustomMail }

procedure TCustomMail.SetConnected(AValue: boolean);
begin
  if fConnected = AValue then
    Exit;
  fIMAPClient.TargetHost := IMAPHost;
  fIMAPClient.TargetPort := IntToStr(IMAPPort);
  fIMAPClient.UserName := Login;
  fIMAPClient.Password := fPassword;
  fConnected := fIMAPClient.Login;
end;

function TCustomMail.GetFullResult: string;
begin
  Result := fIMAPClient.ResultString;
end;

procedure TCustomMail.GetFolderList(var ListFolders: TStringList);
begin
  fIMAPClient.List('', ListFolders);
end;

constructor TCustomMail.Create;
begin
  inherited Create;
  fIMAPClient := TIMAPSend.Create;
  fIMAPClient.AutoTLS := True;
  fIMAPClient.FullSSL := True;
  fConnected := False;
  fLogin := 'i.rcode';
  fPassword := 'LQexIX1CSS';
  fIMAPHost := 'imap.yandex.ru';
  fIMAPPort := 993;
end;

destructor TCustomMail.Destroy;
begin
  fIMAPClient.Logout;
  fIMAPClient.Free;
  inherited Destroy;
end;

end.

