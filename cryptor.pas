unit Cryptor;

{/*

     Модуль Cryptor реализует интерфейс к OpenSSL и Synapse, что позволяет
   шифровать сообщения, отправлять и получать почту...

   Перспективные задачи:

     1. Реализовать протокол аутентификации и обмена информации
     2. Создать хранилище ключей
     3. Сохранение и удаление переписки

*/}
{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, Process, pop3send, smtpsend, mimepart, mimemess,
mimeinln, SynaCode, Dialogs, SQLiteWrap, md5, UserControl;

{ TCrypt }
type
TCrypt = class
private
  UserDB   :TUserList;
  Pop3     :TPOP3Send;
  MailMessage   :TMimeMess;
  fHostPop3, fHostSmtp, fLogin, fPassword   :string;
  fFriendMail   :string;
  fXid     :integer;
public
  constructor Create;
  destructor Destroy; override;
  //Запуск внешних утилит
  function RunApp (Command   :string)   :string;
  //Создание RSA ключей
  function GenRsaPrivateKey (PathToKey   :string; iSize   :integer)   :string;
  function GenRsaPublicKey (PathToPrivKey, PathToPubKey   :string)   :string;
  function GenRsaKeys (PathToPrivKey, PathToPubKey   :string;
    iSize   :integer)   :string;
  //Шифровка и Расшифровка RSA
  function EncryptFile (PathToPubKey, OriginalFile, EncryptedFile   :string)   :string;
  function DecryptFile (PathToPrivKey, CipheredFile, DecodeFile   :string)   :string;
  //Получение почты Pop3
  function Pop3Login (AHost, ALogin, APassword   :string)   :boolean;
  function Pop3Count   :integer; // Количество писем
  function GetMail (Index   :integer)   :string; // Получить исходник письма
  function GetCountSection (Mail   :string)   :integer; // Получить количество секций
  function GetSection (Mail   :string; Index   :integer)   :string;
  // Получить секцию полностью
  function GetSectionHead (Mail   :string; Index   :integer)   :string;
  // Заголовок секции
  function GetSectionBody (Mail   :string; Index   :integer)   :string; // Текст секции
  // Сохранение вложения
  function GetSectionFileName (Mail   :string; Index   :integer)   :string;
  // Имя файла, если это вложение
  function SaveSectionToFile (FileName   :string; Mail   :string;
    Index   :integer)   :boolean; // Сохранение в файла
  function SaveSectionToMemoryStream (MemStream   :TMemoryStream;
    Mail   :string; Index   :integer)   :boolean; // Сохранение в памяти
  // Отправка почты
  function SendMail (pSubject, pTextBody, pHTMLBody   :string;
    Files   :TStrings)   :boolean;
  function KeySend (EMail   :string)   :boolean;      // ???????????
published
  property HostPop3   :string read fHostPop3 write fHostPop3;
  property HostSmtp   :string read fHostSmtp write fHostSmtp;
end;

implementation

{ TCrypt }

constructor TCrypt.Create;
begin
  inherited Create;
  Pop3   := TPOP3Send.Create;
  MailMessage := TMimeMess.Create;
  //UserDB := TUserList.Create (nil);
end;

destructor TCrypt.Destroy;
begin
  Pop3.Free;
  MailMessage.Free;
  inherited Destroy;
end;

function TCrypt.RunApp (Command   :string)   :string;
const
  READ_BYTES = 2048;
var
  S :TStringList;
  M :TMemoryStream;
  P :TProcess;
  n :longint;
  BytesRead :longint;
begin
  M := TMemoryStream.Create;
  BytesRead := 0;
  P := TProcess.Create (nil);
  P.CommandLine := Command;
  P.Options := [poUsePipes];
  P.Execute;
  while P.Running do
  begin
    M.SetSize (BytesRead + READ_BYTES);
    n := P.Output.Read ((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc (BytesRead, n)
    else
      Sleep (100);
  end;
  repeat
    M.SetSize (BytesRead + READ_BYTES);
    n := P.Output.Read ((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc (BytesRead, n);
  until n <= 0;
  M.SetSize (BytesRead);
  S := TStringList.Create;
  S.LoadFromStream (M);
  Result := S.Text;
  S.Free;
  P.Free;
  M.Free;
end;

function TCrypt.GenRsaPrivateKey (PathToKey   :string; iSize   :integer)   :string;
begin
  // Создание приватного ключа, параметры:
  //          PathToKey - Путь к сохранению сгенирированного ключа, iSize - размер ключа
  Result := RunApp ('/usr/bin/openssl genrsa -out ' + PathToKey +
    ' ' + IntToStr (iSize));
end;

function TCrypt.GenRsaPublicKey (PathToPrivKey, PathToPubKey   :string)   :string;
begin
  // Создание публичного ключа на основе приватного, параметры:
  //          PathToPrivKey - Путь к уже существующему файлу приватного ключа
  //          PathToPubKey  - Путь включая имя файла к сохранению нового открытого ключа
  Result := RunApp ('/usr/bin/openssl rsa -in ' + PathToPrivKey +
    ' -pubout -out ' + PathToPubKey);
end;

function TCrypt.GenRsaKeys (PathToPrivKey, PathToPubKey   :string;
iSize   :integer)   :string;
begin
  //Генерация пары ключей
  Result := GenRsaPrivateKey (PathToPrivKey, iSize);
  Result := Result + GenRsaPublicKey (PathToPrivKey, PathToPubKey);
end;

function TCrypt.EncryptFile (PathToPubKey, OriginalFile, EncryptedFile   :string)
 :string;
begin
  // ШИФРОВАНИЕ RSA, параметры:
  //            PathToPubKey - Путь к чужому открытому ключу
  //            OriginalFile - Путь к оригинальному файлу
  //            EncryptedFile - Путь для сохранения зашифрованного файла
  Result := RunApp ('/usr/bin/openssl rsautl -inkey ' + PathToPubKey +
    '  -in ' + OriginalFile + ' -out ' + EncryptedFile + ' -pubin -encrypt');
end;

function TCrypt.DecryptFile (PathToPrivKey, CipheredFile, DecodeFile   :string)
 :string;

begin
  // РАСШИФРОВКА RSA, параметры:
  //             PathToPrivKey - Путь к своему закрытому ключу
  //             CipheredFile  - Путь к зашифрованному файлу
  //             DecodeFile    - Путь и имя файла для записи расшифровки
  Result := RunApp ('/usr/bin/openssl rsautl -inkey ' + PathToPrivKey +
    ' -in ' + CipheredFile + ' -out ' + DecodeFile + ' -decrypt');
end;

function TCrypt.Pop3Login (AHost, ALogin, APassword   :string)   :boolean;
begin
  Result := False;
  Pop3.TargetHost := AHost;
  Pop3.UserName := ALogin;
  Pop3.Password := APassword;
  if Pop3.Login then
  begin
    Result    := True;
    fHostPop3 := AHost;
    fLogin    := ALogin;
    fPassword := APassword;
  end
  else
    raise Exception.Create ('Соединение не удалось. Проверьте логин/пароль/порт.');
end;

function TCrypt.Pop3Count   :integer;
begin
  if Pop3.Stat then
    Result := Pop3.StatCount
  else
    Result := -1;
end;

function TCrypt.GetMail (Index   :integer)   :string;
begin
  if Pop3.Retr (Index) then
    Result := Pop3.FullResult.Text
  else
    Result := 'Error';
end;

function TCrypt.GetCountSection (Mail   :string)   :integer;
var
  i :integer;
begin
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    Result := MessagePart.GetSubPartCount;
  end;
end;

function TCrypt.GetSection (Mail   :string; Index   :integer)   :string;
var
  i :integer;
begin
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := ''
    else
      try
        MessagePart.GetSubPart (Index).PartBody.Text :=
          DecodeBase64 (MessagePart.GetSubPart (Index).PartBody.Text);
        Result := MessagePart.GetSubPart (Index).Lines.Text;
      finally
      end;
  end;
end;

function TCrypt.GetSectionHead (Mail   :string; Index   :integer)   :string;
var
  i :integer;
begin
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := ''
    else
      try
        Result := MessagePart.GetSubPart (Index).Headers.Text;
      finally
      end;
  end;
end;

function TCrypt.GetSectionBody (Mail   :string; Index   :integer)   :string;
var
  i :integer;
begin
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := ''
    else
      try
        Result := MessagePart.GetSubPart (Index).PartBody.Text;
      finally
      end;
  end;
end;

function TCrypt.GetSectionFileName (Mail   :string; Index   :integer)   :string;
var
  i :integer;
begin
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := ''
    else
      try
        Result := MessagePart.GetSubPart (Index).FileName;
      finally
      end;
  end;
end;

function TCrypt.SaveSectionToFile (FileName   :string; Mail   :string;
Index   :integer)   :boolean;
var
  i :integer;
begin
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := False
    else
      try
        if FileName <> '' then
        begin
          // Расшифрованная секция
          MessagePart.GetSubPart (Index).PartBody.Text :=
            DecodeBase64 (MessagePart.GetSubPart (Index).PartBody.Text);
          MessagePart.GetSubPart (Index).PartBody.SaveToFile (FileName); // Сохранение
          Result := True;
        end
        else
          Result := False;
      except
        Result := False;
      end;
  end;
end;

function TCrypt.SaveSectionToMemoryStream (MemStream   :TMemoryStream;
Mail   :string; Index   :integer)   :boolean;

var
  i :integer;
begin
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := False
    else
      try
        // Расшифрованная секция
        MessagePart.GetSubPart (Index).PartBody.Text :=
          DecodeBase64 (MessagePart.GetSubPart (Index).PartBody.Text);
        MessagePart.GetSubPart (Index).PartBody.SaveToStream (MemStream); // Сохранение
        Result := True;
      except
        Result := False;
      end;
  end;
end;

function TCrypt.SendMail (pSubject, pTextBody, pHTMLBody   :string;
Files   :TStrings)   :boolean;
var
  tmpMsg :TMimeMess;
  tmpStringList :TStringList;
  tmpMIMEPart :TMimePart;
  fItem :string;
begin
  tmpMsg := TMimeMess.Create;
  tmpStringList := TStringList.Create;
  Result := False;
  try
    // Headers
    tmpMsg.Header.Subject := pSubject;
    tmpMsg.Header.From    := fLogin;
    tmpMsg.Header.ToList.Add (fFriendMail);
    // MIMe Parts
    tmpMIMEPart := tmpMsg.AddPartMultipart ('alternate', nil);
    if length (pTextBody) > 0 then
    begin
      tmpStringList.Text := pTextBody;
      tmpMsg.AddPartText (tmpStringList, tmpMIMEPart);
    end
    else
    begin
      tmpStringList.Text := pHTMLBody;
      tmpMsg.AddPartHTML (tmpStringList, tmpMIMEPart);
    end;
    for fItem in Files do
      tmpMsg.AddPartBinaryFromFile (fItem, tmpMIMEPart);
    // кодируем и отправляем
    tmpMsg.EncodeMessage;
    Result := smtpsend.SendToRaw (fLogin, fFriendMail, fHostSmtp,
      tmpMsg.Lines, fLogin, fPassword);
  finally
    tmpMsg.Free;
    tmpStringList.Free;
  end;
end;

function TCrypt.KeySend (EMail   :string)   :boolean;
var
  Path  :string;
  Files :TStringList;
begin
  fFriendMail := EMail;
  Files := TStringList.Create;
  Files.Append ('privXID.pem');
  Files.Append ('pubXID.pem');

  GenRsaKeys (Files[0], Files[1], 1024);
  SendMail ('{86AE072A-941F-408A-BD99-4C2E4845C291}',
    '{65E5DFD8-AED9-4A8F-B2EE-F780373AC3B1}', '',
    Files);
  Files.Free;
end;

end.
