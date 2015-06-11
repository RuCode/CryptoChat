{/////////////////////////////////////////////////////////////////////////////////////////////////////////////
                            Модуль для управления почтой по средствам IMAP
 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  11.06.2015 - А что если загружать заголовок\тело и с ним работать остальными методами класса?!
  05.2015 - RuCode
    * Нвчал разработку
 /////////////////////////////////////////////////////////////////////////////////////////////////////////////}

unit Engine.Mail;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, ExtCtrls, Forms, Controls, imapsend, ssl_openssl, synautil, synacode, synaicnv,
  mimemess, mimepart, synachar;

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
    fUserName: string;
    fPassword: string;
    fIMAPHost: string;
    fIMAPPort: integer;
    fIMAPClient: TIMAPSend;
    {:UIID выбранной директории}
    fCurrentFolderUIID: integer;
    function GetCountOfMails: integer;
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
    function SendMail(FromAddr, ToAddr, Subject, Text, FileName: string): boolean;

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

function TCustomMail.SendMail(FromAddr, ToAddr, Subject, Text, FileName: string): boolean;
  // Отправка письма
var
  MailMessage: TMimeMess;
  MIMEPart: TMimePart;
  StringList: TStringList;
begin
  Result:= False;
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

    // Кодируем и отправляем
    MailMessage.EncodeMessage;
    Result := fIMAPClient.AppendMess('Sent', MailMessage.Lines);
  finally
    MailMessage.Free;
    StringList.Free;
  end;
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
end;

destructor TCustomMail.Destroy;
  // Уничтожение обьекта
begin
  fIMAPClient.Logout;
  fIMAPClient.Free;
  inherited Destroy;
end;

end.
