{/////////////////////////////////////////////////////////////////////////////////////////////////////////////
                            Модуль для управления почтой по средствам IMAP
 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  13.06.2015 - К сожалению использовать IMAP для чтения и отправки писем это какой то геморой, добавляю
                  поддержку SMTP. Настройки для яндекса:


                      Входящая почта

                              Протокол — IMAP;
                              Имя сервера — imap.yandex.ru;
                              Порт — 993;
                              SSL — SSL/TLS;
                              Аутентификация — Обычный пароль.

                      Исходящая почта

                              Имя сервера — smtp.yandex.ru;
                              Порт — 465;
                              SSL — SSL/TLS;
                              Аутентификация — Обычный пароль.

             - Так же идея в том, что грузим сначала заголовок письма, а дальше применяем к нему методы и
                  если заголовка не хватает, то догружаем тело...


  11.06.2015 - А что если загружать заголовок\тело и с ним работать остальными методами класса?!
  05.2015 - RuCode
    * Нвчал разработку
 /////////////////////////////////////////////////////////////////////////////////////////////////////////////}

unit Engine.Mail;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, ExtCtrls, Forms, Controls, imapsend, ssl_openssl, synautil, synacode, synaicnv,
  mimemess, mimepart, synachar, smtpsend;

const
  ERROR_CAPTION = 'Ошибка';
  ERROR_INDEX_MESSAGE = 'Индекс сообщения должен быть от 1 до числа равного количеству писем...';
  ERROR_RECIEVE_HEADER = 'Ошибка получения заголовка письма #%d';
  ERROR_RECIEVE_BODY = 'Ошибка получения тела письма #%d';

type

  { TCustomMail }

  TCustomMail = class(TObject)
  private
    fConnected: boolean;
    {:Для хранения полного ответа на нашу команду}
    fFullResult: TStringList;
    {:Для чтения почты по протоколу IMAP}
    fIMAPClient: TIMAPSend;
    {:Для отправки почты по протоколу SMTP}
    fSMTPClient: TSMTPSend;

    function GetCountOfMails: integer;
    function GetCountOfNewMails: integer;
    function GetFullResult: TStringList;
    function GetIMAPHost: string;
    function GetIMAPPort: integer;
    function GetPassword: string;
    function GetResultString: string;
    function GetSelectedFolder: string;
    function GetSMTPHost: string;
    function GetSMTPPort: integer;
    function GetUserName: string;
    procedure SetConnected(AValue: boolean);
    procedure SetIMAPHost(AValue: string);
    procedure SetIMAPPort(AValue: integer);
    procedure SetPassword(AValue: string);
    procedure SetSelectedFolder(AFolderName: string);
    procedure SetSMTPHost(AValue: string);
    procedure SetSMTPPort(AValue: integer);
    procedure SetUserName(AValue: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    {:Узнать список директорий на сервере.}
    procedure GetFolderList(var ListFolders: TStringList);
    {:Получить заголовок сообщения.}
    function GetMailHeader(Index: integer): string;
    {:Получить сообщение.}
    function GetMailBody(Index: integer): string;
    {:Получить текст сообщения в расшифрованном виде}
    function GetMailText(Text: TStringList): string;
    {:Получить количество вложений}
    function GetMailAttachCount(Text: TStringList): integer;
    {:Получить имя файла вложения}
    function GetMailAttachFileName(Text: TStringList; Index: integer): string;
    {:Сохранить вложение в файо}
    procedure SaveAttachToFile(AMailIndex, AttachIndex: integer; AFileName: string);
    {:Узнать почту отправителя}
    function GetEmailFrom(AMailIndex: integer): string;
    {:Узнать почту получателя}
    function GetEmailTo(AMailIndex: integer): string;
    {:Тема письма}
    function GetEmailSubject(AMailIndex: integer): string;
    {:Дата отправки письма}
    function GetEmailDate(AMailIndex: integer): TDateTime;
    {:Отправка письма}
    function SendMail(FromAddr, ToAddr, Subject, Text: string; FileName: string = ''): boolean;

  public
    {:Имя пользователя для авторизации.}
    property UserName: string read GetUserName write SetUserName;
    {:Пароль для авторизации.}
    property Password: string read GetPassword write SetPassword;
    {:Адрес сервера IMAP.}
    property IMAPHost: string read GetIMAPHost write SetIMAPHost;
    {:Порт на котором вертиться протокол IMAP.}
    property IMAPPort: integer read GetIMAPPort write SetIMAPPort;
    {:Адрес сервера SMTP.}
    property SMTPHost: string read GetSMTPHost write SetSMTPHost;
    {:Порт на котором вертиться протокол SMTP.}
    property SMTPPort: integer read GetSMTPPort write SetSMTPPort;
    {:Управление соединением и получение состояния соединения.}
    property Connected: boolean read fConnected write SetConnected;
    {:Узнать или указать текущую директорию.}
    property SelectedFolder: string read GetSelectedFolder write SetSelectedFolder;
    {:Количество новых писем}
    property CountOfNewMails: integer read GetCountOfNewMails;
    {:Количество писем в директории}
    property CountOfMails: integer read GetCountOfMails;
    {:Результаты команды.}
    property FullResult: TStringList read GetFullResult;
    {:Результаты команды.}
    property ResultString: string read GetResultString;
  end;

implementation

{ TCustomMail }

procedure TCustomMail.SetConnected(AValue: boolean);
// Устанавливаем или сбрасываем соединение с сервером
var
  SMTPConnected, IMAPConnected: boolean;
begin
  if fConnected = AValue then
    Exit;
  if AValue then
  begin
    IMAPConnected := fIMAPClient.Login;
    SMTPConnected := fSMTPClient.Login;
    fConnected := IMAPConnected and SMTPConnected;
    if not fConnected then
    begin
      if SMTPConnected then
        fSMTPClient.Logout;
      if IMAPConnected then
        fIMAPClient.Logout;
    end;
  end
  else
  begin
    fIMAPClient.Logout;
    fSMTPClient.Logout;
    fConnected := False;
  end;
end;

procedure TCustomMail.SetIMAPHost(AValue: string);
// Установить имя сервера IMAP
begin
  fIMAPClient.TargetHost := AValue;
end;

procedure TCustomMail.SetIMAPPort(AValue: integer);
// Установить порт подключения к серверу IMAP
begin
  fIMAPClient.TargetPort := IntToStr(AValue);
end;

procedure TCustomMail.SetPassword(AValue: string);
// Устанавливаем пароль для авторизации
begin
  fIMAPClient.Password := AValue;
  fSMTPClient.Password := AValue;
end;

procedure TCustomMail.SetSelectedFolder(AFolderName: string);
// Выбираем новую директорию
begin
  fIMAPClient.SelectFolder(AFolderName);
end;

procedure TCustomMail.SetSMTPHost(AValue: string);
// Установить имя сервера SMTP
begin
  fSMTPClient.TargetHost := AValue;
end;

procedure TCustomMail.SetSMTPPort(AValue: integer);
// Установить порт сервера SMTP
begin
  fSMTPClient.TargetPort := IntToStr(AValue);
end;

procedure TCustomMail.SetUserName(AValue: string);
// Устанавливаем имя пользователя
begin
  fIMAPClient.UserName := AValue;
  fSMTPClient.UserName := AValue;
end;

function TCustomMail.GetFullResult: TStringList;
  // Получим полный ответ от сервера
begin
  fFullResult.Assign(fIMAPClient.FullResult);
  fFullResult.Add(fSMTPClient.FullResult.Text);
  Result := fFullResult;
end;

function TCustomMail.GetIMAPHost: string;
  // Получить имя сервера IMAP
begin
  Result := fIMAPClient.TargetHost;
end;

function TCustomMail.GetIMAPPort: integer;
  // Получить порт сервера IMAP
begin
  Result := StrToInt(fIMAPClient.TargetPort);
end;

function TCustomMail.GetPassword: string;
  // Получить пароль
begin
  if fIMAPClient.Password <> fSMTPClient.Password then
    raise Exception.Create('Фатальная ошибка: Пароль пользователя должен быть одинаковым и для входящей и для исходящей почты...');
  Result := fIMAPClient.Password;
end;

function TCustomMail.GetCountOfNewMails: integer;
  // Количество новых писем
begin
  Result := fIMAPClient.SelectedRecent;
end;

function TCustomMail.GetCountOfMails: integer;
  // Количество писем
begin
  Result := fIMAPClient.SelectedCount;
end;

function TCustomMail.GetResultString: string;
  // Статус команды
begin
  Result := fIMAPClient.ResultString + #13 + #10 + fSMTPClient.ResultString;
end;

function TCustomMail.GetSelectedFolder: string;
  // Определить текущею директорию
begin
  Result := fIMAPClient.SelectedFolder;
end;

function TCustomMail.GetSMTPHost: string;
  // Получить имя сервера SMTP
begin
  Result := fSMTPClient.TargetHost;
end;

function TCustomMail.GetSMTPPort: integer;
  // Получить порт SMTP
begin
  Result := StrToInt(fSMTPClient.TargetPort);
end;

function TCustomMail.GetUserName: string;
  // Получить имя пользователя
begin
  if fIMAPClient.UserName <> fSMTPClient.UserName then
    raise Exception.Create('Фатальная ошибка: Имя пользователя должно быть одинаковым и для входящей и для исходящей почты...');
  Result := fIMAPClient.UserName;
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

function TCustomMail.GetMailHeader(Index: integer): string;
  // Получить заголовок сообщения
var
  List: TStringList;
begin
  Result := '';
  if Index < 1 then
  begin
    MessageDlg(ERROR_CAPTION, ERROR_INDEX_MESSAGE, mtError, [mbOK], '');
    Exit;
  end;
  try
    List := TStringList.Create;
    if not fIMAPClient.FetchHeader(Index, List) then
    begin
      MessageDlg(ERROR_CAPTION, Format(ERROR_RECIEVE_HEADER, [Index]), mtError, [mbOK], '');
      Exit;
    end;
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TCustomMail.GetMailBody(Index: integer): string;
  // Получить заголовок сообщения
var
  List: TStringList;
begin
  Result := '';
  if Index < 1 then
  begin
    MessageDlg(ERROR_CAPTION, ERROR_INDEX_MESSAGE, mtError, [mbOK], '');
    Exit;
  end;
  try
    List := TStringList.Create;
    if not fIMAPClient.FetchMess(Index, List) then
    begin
      MessageDlg(ERROR_CAPTION, Format(ERROR_RECIEVE_BODY, [Index]), mtError, [mbOK], '');
      Exit;
    end;
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TCustomMail.GetMailText(Text: TStringList): string;
  // Получить текст письма из тела письма
var
  MimeMess: TMimemess;
begin
  MimeMess := TMimemess.Create;
  MimeMess.Lines.Assign(Text);
  MimeMess.DecodeMessage;
  Result := MimeMess.MessagePart.PartBody.Text;
  MimeMess.Free;
end;

function TCustomMail.GetMailAttachCount(Text: TStringList): integer;
  // Получить количество вложений
var
  MimeMess: TMimemess;
begin
  MimeMess := TMimemess.Create;
  MimeMess.Lines.Assign(Text);
  MimeMess.DecodeMessage;
  Result := MimeMess.MessagePart.GetSubPartCount;
  MimeMess.Free;
end;

function TCustomMail.GetMailAttachFileName(Text: TStringList; Index: integer): string;
  // Получить имя файла вложения
var
  MimeMess: TMimeMess;
  MimePart: TMimePart;
begin
  MimeMess := TMimemess.Create;
  MimeMess.Lines.Assign(Text);
  MimeMess.DecodeMessage;
  MimePart := MimeMess.MessagePart.GetSubPart(Index);
  MimePart.DecodePart;
  Result := MimePart.FileName;
  MimeMess.Free;
end;

procedure TCustomMail.SaveAttachToFile(AMailIndex, AttachIndex: integer; AFileName: string);
// Cохраняет вложение в файл

  procedure SaveToFile(szData, szPath: string);
  var
    hFile: TextFile;
  begin
    AssignFile(hFile, szPath);
    Rewrite(hFile);
    Write(hFile, szData);
    CloseFile(hFile);
  end;

const
  ATTACHMENT_STR = 'attachment';

var
  MimePart: TMimePart;
  i, CountAttach: integer;
  Stream: TStringStream;
  MailMessage: TMimeMess;
begin
  MailMessage := TMimeMess.Create;
  CountAttach := -1;
  if not Connected then
    Exit;
  MailMessage.Lines.Text := GetMailBody(AMailIndex);
  MailMessage.DecodeMessage;
  for i := 0 to MailMessage.MessagePart.GetSubPartCount - 1 do
  begin
    Inc(CountAttach, 1);
    MimePart := MailMessage.MessagePart.GetSubPart(i);
    if not SameText(MimePart.Disposition, ATTACHMENT_STR) then
      Continue;
    if CountAttach <> AttachIndex then
      Continue;
    Stream := TStringStream.Create('');
    try
      Stream.WriteString(DecodeBase64(MimePart.PartBody.Text));
      SaveToFile(Stream.DataString, AFileName);
    finally
      Stream.Free;
      MailMessage.Free;
    end;
  end;
end;

function TCustomMail.GetEmailFrom(AMailIndex: integer): string;
  // Узнать кем отправлено письмо
var
  MailMessage: TMimeMess;
begin
  MailMessage := TMimeMess.Create;
  MailMessage.Lines.Text := GetMailHeader(AMailIndex);
  MailMessage.DecodeMessage;
  Result := MailMessage.Header.From;
  MailMessage.Free;
end;

function TCustomMail.GetEmailTo(AMailIndex: integer): string;
  // Узнать кем отправлено письмо
var
  MailMessage: TMimeMess;
begin
  MailMessage := TMimeMess.Create;
  MailMessage.Lines.Text := GetMailHeader(AMailIndex);
  MailMessage.DecodeMessage;
  Result := MailMessage.Header.ToList[0];
  MailMessage.Free;
end;

function TCustomMail.GetEmailSubject(AMailIndex: integer): string;
  // Узнать тему письма
var
  MailMessage: TMimeMess;
begin
  MailMessage := TMimeMess.Create;
  MailMessage.Lines.Text := GetMailHeader(AMailIndex);
  MailMessage.DecodeMessage;
  Result := MailMessage.Header.Subject;
  MailMessage.Free;
end;

function TCustomMail.GetEmailDate(AMailIndex: integer): TDateTime;
  // Дата отправки письма
var
  MailMessage: TMimeMess;
begin
  MailMessage := TMimeMess.Create;
  MailMessage.Lines.Text := GetMailHeader(AMailIndex);
  MailMessage.DecodeMessage;
  Result := MailMessage.Header.Date;
  MailMessage.Free;
end;

function TCustomMail.SendMail(FromAddr, ToAddr, Subject, Text: string; FileName: string = ''): boolean;
  // Отправка письма
var
  MailMessage: TMimeMess;
  MIMEPart: TMimePart;
  StringList: TStringList;
begin
  Result := False;
  MailMessage := TMimeMess.Create;
  StringList := TStringList.Create;
  try
    // Заголовок письма
    MailMessage.Header.Subject := Subject;// тема сообщения
    MailMessage.Header.From := FromAddr; // имя и адрес отправителя
    MailMessage.Header.ToList.Add(ToAddr); // имя и адрес получателя
    // Корневой элемент
    MIMEPart := MailMessage.AddPartMultipart('alternative', nil);
    if length(Text) <> 0 then
    begin
      StringList.Text := Text;
      MailMessage.AddPartText(StringList, MIMEPart);
    end;
    // Вложение
    if FileName <> '' then
      MailMessage.AddPartBinaryFromFile(FileName, MIMEPart);
    // Кодируем и отправляем
    MailMessage.EncodeMessage;
    fSMTPClient.MailFrom(FromAddr, Length(FromAddr));
    fSMTPClient.MailTo(ToAddr);
    fSMTPClient.MailData(MailMessage.Lines);
  finally
    MailMessage.Free;
    StringList.Free;
  end;
end;

constructor TCustomMail.Create;
  // Создание обьекта
begin
  inherited Create;
  // IMAP
  fIMAPClient := TIMAPSend.Create;
  fIMAPClient.AutoTLS := True;
  fIMAPClient.FullSSL := True;
  IMAPHost := 'imap.yandex.ru';
  IMAPPort := 993;
  // SMTP
  fSMTPClient := TSMTPSend.Create;
  fSMTPClient.AutoTLS := True;
  fSMTPClient.FullSSL := True;
  SMTPHost := 'smtp.yandex.ru';
  SMTPPort := 465;
  // Инициализация
  fFullResult := TStringList.Create;
  fConnected := False;
  UserName := 'i.rcode';
  Password := 'LQexIX1';
end;

destructor TCustomMail.Destroy;
  // Уничтожение обьекта
begin
  fFullResult.Free;
  fSMTPClient.Logout;
  fSMTPClient.Free;
  fIMAPClient.Logout;
  fIMAPClient.Free;
  inherited Destroy;
end;

end.
