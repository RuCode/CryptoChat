{/////////////////////////////////////////////////////////////////////////////////////////////////////////////
                            Модуль для управления почтой по средствам IMAP
 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  05.2015 - RuCode
    * Нвчал разработку
 /////////////////////////////////////////////////////////////////////////////////////////////////////////////}

unit Engine.Mail;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, ExtCtrls, Forms, Controls, imapsend, ssl_openssl, synautil, synacode, synaicnv,
  mimeinln, mimemess, mimepart, synachar;

type

  { TCustomMail }

  TCustomMail = class(TObject)
  private
    fConnected: boolean;
    fUserName: string;
    fPassword: string;
    fIMAPHost: string;
    fIMAPPort: integer;
    fIMAPClient: TIMAPSend;
    {:UIID выбранной директории}
    fCurrentFolderUIID: integer;
    {:Количество новых писем в текущей директории}
    fCountOfNewMails: integer;
    {:Количество писем в текущей директории}
    fCountOfMailsInCurrentFolder: integer;
    function GetCountOfNewMails: integer;
    function GetFullResult: TStringList;
    function GetResultString: string;
    function GetSelectedFolder: string;
    procedure SetConnected(AValue: boolean);
    procedure SetSelectedFolder(AFolderName: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    {:Узнать список директорий на сервере.}
    procedure GetFolderList(var ListFolders: TStringList);
    {:Узнать список директорий на сервере.}

  public
    {:Имя пользователя для авторизации.}
    property UserName: string read fUserName write fUserName;
    {:Пароль для авторизации.}
    property Password: string write fPassword;
    {:Адрес сервера IMAP.}
    property IMAPHost: string read fIMAPHost write fIMAPHost;
    {:Порт на котором вертиться протокол IMAP.}
    property IMAPPort: integer read fIMAPPort write fIMAPPort;
    {:Управление соединением и получение состояния соединения.}
    property Connected: boolean read fConnected write SetConnected;
    {:Узнать или указать текущую директорию.}
    property SelectedFolder: string read GetSelectedFolder write SetSelectedFolder;
    {: Количество новых писем}
    property CountOfNewMails: integer read GetCountOfNewMails;
    {:Результаты команды.}
    property FullResult: TStringList read GetFullResult;
    {:Результаты команды.}
    property ResultString: string read GetResultString;
  end;

implementation

{ TCustomMail }

procedure TCustomMail.SetConnected(AValue: boolean);
// Устанавливаем или сбрасываем соединение с сервером
begin
  if fConnected = AValue then
    Exit;
  if AValue then
  begin
    fIMAPClient.TargetHost := IMAPHost;
    fIMAPClient.TargetPort := IntToStr(IMAPPort);
    fIMAPClient.UserName := UserName;
    fIMAPClient.Password := fPassword;
    fConnected := fIMAPClient.Login;
  end
  else
  begin
    fIMAPClient.Logout;
    fConnected := False;
  end;
end;

procedure TCustomMail.SetSelectedFolder(AFolderName: string);
// Выбираем новую директорию
begin
  if fIMAPClient.SelectFolder(AFolderName) then
  begin
    fCountOfMailsInCurrentFolder := fIMAPClient.SelectedCount;
    fCountOfNewMails := fIMAPClient.SelectedRecent;
    fCurrentFolderUIID := fIMAPClient.SelectedUIDvalidity;
  end;
end;

function TCustomMail.GetFullResult: TStringList;
// Получим полный ответ от сервера
begin
  Result := fIMAPClient.FullResult;
end;

function TCustomMail.GetCountOfNewMails: integer;
// Количество новых писем
var
  MessList: TStringList;
begin
  MessList := TStringList.Create;
  if ImapClient.SearchMess('UNSEEN', MessList) then
  begin
    Result := MessList.Count;
    fCountOfNewMails := Result;
  end;
  MessList.Free;
end;

function TCustomMail.GetResultString: string;
// Статус команды
begin
  Result := fIMAPClient.ResultString;
end;

function TCustomMail.GetSelectedFolder: string;
// Определить текущею директорию
begin
  Result := fIMAPClient.SelectedFolder;
end;

procedure TCustomMail.GetFolderList(var ListFolders: TStringList);
// Получить список директорий на сервере
var
  i: integer;
begin
  fIMAPClient.List('', ListFolders);
  for i := 0 to ListFolders.Count - 1 do
    ListFolders[i] := CharsetConversion(ListFolders[i], TMimeChar.UTF_7mod, TMimeChar.UTF_8);
end;

constructor TCustomMail.Create;
// Создание обьекта
begin
  inherited Create;
  fIMAPClient := TIMAPSend.Create;
  fIMAPClient.AutoTLS := True;
  fIMAPClient.FullSSL := True;
  fConnected := False;
  fUserName := 'i.rcode';
  fPassword := 'LQexIX1';
  fIMAPHost := 'imap.yandex.ru';
  fIMAPPort := 993;
  fCurrentFolderUIID := 0;
  fCountOfNewMails := 0;
  fCountOfMailsInCurrentFolder := 0;
end;

destructor TCustomMail.Destroy;
// Уничтожение обьекта
begin
  fIMAPClient.Logout;
  fIMAPClient.Free;
  inherited Destroy;
end;

end.
