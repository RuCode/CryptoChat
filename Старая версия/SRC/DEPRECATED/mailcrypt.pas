unit MailCrypt;

{$mode objfpc}{$H+}

interface

uses
  Classes, MainCrypt,{ mimeinln,} mimemess, mimepart, pop3send, smtpsend, Dialogs,
  SysUtils, ExtCtrls, Forms, Controls, synacode;

const
  RET_ERROR_STR  = 'Error';
  ATTACHMENT_STR = 'attachment';

type

  { TMailCrypt }

  TMailCrypt = class(TMainCrypt)
  private
    fMailHead: boolean;
    fMailAuth: boolean;
    fMailText: TStringList;
    fMailIndex: integer;
    fHost: string;
    Pop3: TPOP3Send;
    MailMessage: TMimeMess;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //* ======================================================================== *//
    function ConnectMail: boolean;
    function SendMail(pHost, pSubject, pTo, pFrom, pTextBody, pHTMLBody: string;
      files: TStrings): boolean;
    function GetMailCount: integer;
    function GetMail(AIndex: integer): string;
    function GetMailHead(AIndex: integer): string;
    procedure LoadMailFromText(AIndex: integer; AText: string);
    //* ======================================================================== *//
    // Кем отправлено
    function GetMailFrom(AIndex: integer): string;
    // Кому отпралено
    function GetMailTo(AIndex: integer): string;
    // Тема письма
    function GetMailSubject(AIndex: integer): string;
    // Время отправки
    function GetMailDate(AIndex: integer): TDateTime;
    //* ======================================================================== *//
    function GetAttachCount(AIndex: integer): integer;
    function GetAttachName(AMailIndex, AIndex: integer): string;
    function SaveAttachToFile(AMailIndex, AIndex: integer; AFileName: string): boolean;
    //* ======================================================================== *//
    function FindMail(AFriendMail: string; AStartIndex: integer): integer;
    function ExtractEmail(AText: string): string;
  public
    function AddFriend(AFriend: TFriendEntry): boolean; override;
    function GetMailCommand(Index: integer): string;
  published
    property Host: string Read fHost Write fHost;
  end;

implementation

{ TMailCrypt }

constructor TMailCrypt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Pop3      := TPOP3Send.Create;
  MailMessage := TMimeMess.Create;
  fMailAuth := False;
  fMailText := TStringList.Create;
  fMailIndex := -1;
  fMailHead := False;
end;

destructor TMailCrypt.Destroy;
begin
  MailMessage.Free;
  fMailText.Free;
  Pop3.Free;
  inherited Destroy;
end;

function TMailCrypt.ConnectMail: boolean;
var
  UsrInf: TUserEntry;
begin
  Result := False;
  UsrInf := GetUserInfo;
  Pop3.TargetHost := fHost;
  Pop3.UserName := UsrInf.Email;
  Pop3.Password := UsrInf.HashPwd;
  if Pop3.Login then
    Result := True
  else
    raise Exception.Create('Соединение не удалось. Проверьте логин/пароль/порт.');
  fMailAuth := Result;
end;

function TMailCrypt.SendMail(pHost, pSubject, pTo, pFrom, pTextBody, pHTMLBody: string;
  files: TStrings): boolean;
var
  tmpMsg: TMimeMess;
  tmpStringList: TStringList;
  tmpMIMEPart: TMimePart;
  fItem:  string;
  UsrInf: TUserEntry;
begin
  Result := True;
  tmpMsg := TMimeMess.Create;
  tmpStringList := TStringList.Create;
  try
    try
      tmpMsg.Header.Subject := pSubject;
      tmpMsg.Header.From    := pFrom;
      tmpMsg.Header.ToList.Add(pTo);
      tmpMIMEPart := tmpMsg.AddPartMultipart('alternate', nil);
      if length(pTextBody) > 0 then
      begin
        tmpStringList.Text := pTextBody;
        tmpMsg.AddPartText(tmpStringList, tmpMIMEPart);
      end
      else
      begin
        tmpStringList.Text := pHTMLBody;
        tmpMsg.AddPartHTML(tmpStringList, tmpMIMEPart);
      end;
      for fItem in files do
        tmpMsg.AddPartBinaryFromFile(fItem, tmpMIMEPart);
      tmpMsg.EncodeMessage;
      UsrInf := GetUserInfo;
      smtpsend.SendToRaw(pFrom, pTo, pHost, tmpMsg.Lines, UsrInf.Email, UsrInf.HashPwd);
    except
      Result := False;
    end;
  finally
    tmpMsg.Free;
    tmpStringList.Free;
  end;
end;

function TMailCrypt.GetMailCount: integer;
begin
  Result := -1;
  if fMailAuth then
    if Pop3.Stat then
      Result := Pop3.StatCount;
end;

function TMailCrypt.GetMail(AIndex: integer): string;
begin
  Result := RET_ERROR_STR;
  if fMailAuth then
    if Pop3.Retr(AIndex) then
    begin
      Result := Pop3.FullResult.Text;
      fMailText.Assign(Pop3.FullResult);
      fMailIndex := AIndex;
      fMailHead  := False;
    end;
end;

function TMailCrypt.GetMailHead(AIndex: integer): string;
begin
  // Разделить Top и Полное письмо
  Result := RET_ERROR_STR;
  if not fMailAuth then
    Exit;
  if AIndex <> fMailIndex then
    if fMailAuth then
      if Pop3.Top(AIndex, 1) then
      begin
        Result := Pop3.FullResult.Text;
        fMailText.Assign(Pop3.FullResult);
        fMailIndex := AIndex;
        fMailHead  := True;
      end
      else
        exit;
  MailMessage.Lines.Assign(fMailText);
  MailMessage.DecodeMessage;
  Result := MailMessage.Header.CustomHeaders.Text;
end;

procedure TMailCrypt.LoadMailFromText(AIndex: integer; AText: string);
begin
  fMailText.Text := AText;
  fMailIndex     := AIndex;
  fMailHead      := True;
end;

function TMailCrypt.GetMailFrom(AIndex: integer): string;
begin
  Result := RET_ERROR_STR;
  if not fMailAuth then
    Exit;
  if AIndex <> fMailIndex then
    if GetMailHead(AIndex) = RET_ERROR_STR then
      exit;
  MailMessage.Lines.Assign(fMailText);
  MailMessage.DecodeMessage;
  Result := MailMessage.Header.From;
end;

function TMailCrypt.GetMailTo(AIndex: integer): string;
begin
  Result := RET_ERROR_STR;
  if not fMailAuth then
    Exit;
  if AIndex <> fMailIndex then
    if GetMailHead(AIndex) = RET_ERROR_STR then
      exit;
  MailMessage.Lines.Assign(fMailText);
  MailMessage.DecodeMessage;
  Result := MailMessage.Header.ToList[0];
end;

function TMailCrypt.GetMailSubject(AIndex: integer): string;
begin
  Result := RET_ERROR_STR;
  if not fMailAuth then
    Exit;
  if AIndex <> fMailIndex then
    if GetMailHead(AIndex) = RET_ERROR_STR then
      exit;
  fMailHead:= true;
  MailMessage.Lines.Assign(fMailText);
  MailMessage.DecodeMessage;
  Result := MailMessage.Header.Subject;
end;

function TMailCrypt.GetMailDate(AIndex: integer): TDateTime;
begin
  Result := TDateTime(0);
  if not fMailAuth then
    Exit;
  if AIndex <> fMailIndex then
    if GetMailHead(AIndex) = RET_ERROR_STR then
      exit;
  MailMessage.Lines.Assign(fMailText);
  MailMessage.DecodeMessage;
  Result := MailMessage.Header.Date;
end;

function TMailCrypt.GetAttachCount(AIndex: integer): integer;
var
  MimePart: TMimePart;
  i: integer;
begin
  Result := -1;
  if not fMailAuth then
    Exit;
  if (AIndex <> fMailIndex) and not fMailHead then
    if GetMail(AIndex) = RET_ERROR_STR then
      exit;
  MailMessage.Lines.Assign(fMailText);
  MailMessage.DecodeMessage;
  for i := 0 to MailMessage.MessagePart.GetSubPartCount - 1 do
  begin
    MimePart := MailMessage.MessagePart.GetSubPart(i);
    if SameText(MimePart.Disposition, ATTACHMENT_STR) then
      Inc(Result, 1);
    //       ShowMessage(MimePart.Disposition);
  end;
end;

function TMailCrypt.GetAttachName(AMailIndex, AIndex: integer): string;
var
  MimePart: TMimePart;
  i, CountAttach: integer;
begin
  Result      := RET_ERROR_STR;
  CountAttach := -1;
  if not fMailAuth then
    Exit;
  if (AMailIndex <> fMailIndex) and not fMailHead then
    if GetMail(AMailIndex) = RET_ERROR_STR then
      exit;
  MailMessage.Lines.Assign(fMailText);
  MailMessage.DecodeMessage;
  for i := 0 to MailMessage.MessagePart.GetSubPartCount - 1 do
  begin
    MimePart := MailMessage.MessagePart.GetSubPart(i);
    if SameText(MimePart.Disposition, ATTACHMENT_STR) then
    begin
      Inc(CountAttach, 1);
      if CountAttach = AIndex then
        Result := MimePart.FileName;
    end;
  end;
end;

function TMailCrypt.SaveAttachToFile(AMailIndex, AIndex: integer;
  AFileName: string): boolean;

  procedure SaveToFile(szData, szPath: string);
  var
    hFile: TextFile;
  begin
    AssignFile(hFile, szPath);
    Rewrite(hFile);
    Write(hFile, szData);
    CloseFile(hFile);
  end;

var
  MimePart: TMimePart;
  i, CountAttach: integer;
  Stream:   TStringStream;
begin
  Result      := False;
  CountAttach := -1;
  if not fMailAuth then
    Exit;
  if (AMailIndex <> fMailIndex) and not fMailHead then
    if GetMail(AIndex) = RET_ERROR_STR then
      exit;
  MailMessage.Lines.Assign(fMailText);
  MailMessage.DecodeMessage;
  for i := 0 to MailMessage.MessagePart.GetSubPartCount - 1 do
  begin
    MimePart := MailMessage.MessagePart.GetSubPart(i);
    if SameText(MimePart.Disposition, ATTACHMENT_STR) then
    begin
      Inc(CountAttach, 1);
      if CountAttach = AIndex then
      begin
        Stream := TStringStream.Create('');
        try
          Stream.WriteString(DecodeBase64(MimePart.PartBody.Text));
          SaveToFile(Stream.DataString, AFileName);
          // Stream.SaveToFile(ExtractFilePath(Application.ExeName) + '/Mail/'+ Sender.FileName);
        finally
          Stream.Free;
          Result:= True;
        end;
      end;
    end;
  end;
end;

function TMailCrypt.FindMail(AFriendMail: string; AStartIndex: integer): integer;
var
  i, unCount: integer; // UnUsed mails
begin
  Result  := -1;
  unCount := 0;
  for i := AStartIndex to GetMailCount do
    if (pos(AFriendMail, GetMailFrom(i)) <> 0) or
      (pos(AFriendMail, GetMailTo(i)) <> 0) then
    begin
      Result  := i;
      unCount := -1;
      break;
    end
    else if unCount > 20 then
      break
    else
      Inc(unCount, 1);
end;

function TMailCrypt.ExtractEmail(AText: string): string;
var
  ch:   char;
  flag: boolean;
begin
  Result := '';
  flag   := False;
  for ch in AText do
  begin
    if ch = '>' then
      flag := False;
    if Flag then
      Result += ch;
    if ch = '<' then
      flag := True;
  end;
  if Result = '' then
    Result := AText;
end;

//*****************************************************************************//

//       Задачи ниже лежащго кода сводятся к обмену по сети информацией

//*****************************************************************************//

function TMailCrypt.AddFriend(AFriend: TFriendEntry): boolean;
const
  // Сообщения всегда имеют эту тему
  CHAT_MAIN_SUBJECT  = '{6D78D26F-347E-4D57-9E9C-03C82139CD38}';
  // Имя открытого ключа
  CHAT_NAME_OPEN_KEY = '{5898E436-3B1D-47D1-86CC-2FFF6C27FF77}';
  // Запрос на добавления в друзья
  CHAT_REQUEST_ADD_FRIEND = '{138C7A7C-8F74-4DAF-838B-21E6842A031D}';
var
  files: TStringList;
  PathSec, PathOpn: string;
  Amsg:  TMessageEntry;
begin
  { TODO : Задачи для TMailCrypt.AddFriend:
         1. Создать паруключей
         2. Отправить пиьмо, содержащее:
            2.1. Открытый ключ
            2.2. Юид отвечающий на запрос о авторизации
         3. Ответ должен содержать:
            3.1. Юид подтверждающий аутентификацию
            3.2. Открытый ключ
            3.3. Первое сообщение (опционально) }
  Files   := TStringList.Create;
  // Генерируем ключи
  PathSec := '/tmp/private.key';
  PathOpn := '/tmp/' + CHAT_NAME_OPEN_KEY;
  GenRsaKeys(PathSec, PathOpn, 2048);
  files.Add('/tmp/' + CHAT_NAME_OPEN_KEY);
  // Стучимся на почтовый сервер
  Host := 'pop.yandex.ru';

  // Отправляем письмо
  if ConnectMail then
  begin
    if not SendMail('smtp.yandex.ru', CHAT_MAIN_SUBJECT, AFriend.Email,
      GetUserInfo.Email, CHAT_REQUEST_ADD_FRIEND, '', files) then
    begin
      raise Exception.Create('Не могу отправить приглашение на аутентификацию');
      abort;
    end;
  end
  else
  begin
    raise Exception.Create('Не могу подключиться к почтовому серверу');
    abort;
  end;

  // Добавляем в бд друга
  Result := inherited AddFriend(AFriend);

  // Добавляем сообщение в бд - нужно будет перенести в MakeFirstMail из MainCrypt
  Amsg.ID_FRIEND := GetCountFriend;
  Amsg.ID_USER := GetUserInfo.ID_USER;
  Amsg.Date := Now;
  Amsg.Files := '';
  Amsg.Message := '';
  Amsg.IsMyMsg := True;
  AMsg.OpenKey := TextReadFile('/tmp/' + CHAT_NAME_OPEN_KEY);
  AMsg.SecretKey := Base64ReadFile('/tmp/private.key');
  AMsg.BFKey := '';
  Amsg.XID := 1;

  if not AddMessage(Amsg) then
    raise Exception.Create('Ошибка добавления сообщения в БД');

  // Заметаем за собой
  DeleteFile('/tmp/private.key');
  DeleteFile('/tmp/open.key');
  Files.Free;
end;

function TMailCrypt.GetMailCommand(Index: integer): string;
var
  MimePart: TMimePart;
  i: integer;
begin
  Result:= '';
  if not fMailAuth then
    Exit;
  if (Index <> fMailIndex) and not fMailHead then
    if GetMail(Index) = RET_ERROR_STR then
      exit;
  MailMessage.Lines.Assign(fMailText);
  MailMessage.DecodeMessage;
  for i := 0 to MailMessage.MessagePart.GetSubPartCount - 1 do
  begin
    MimePart := MailMessage.MessagePart.GetSubPart(i);
    if SameText('inline', MimePart.Disposition) then
    begin
      Result+= MimePart.PartBody.Text;
    end;
  end;
end;

end.
