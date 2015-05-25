unit CustomCrypt;

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
  Classes, Controls,
  Dialogs, mimemess, mimepart, pop3send, Process, smtpsend, Forms,
  SQLiteWrap, SynaCode, SysUtils;

{ TCustomCrypt }
type
  TCustomCrypt = class(TWinControl)
  private
    Pop3: TPOP3Send;
    MailMessage: TMimeMess;
    fHostPop3, fHostSmtp, fLogin, fPassword: string;
    fFriendMail: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //Запуск внешних утилит
    function RunApp(Command: string): string; overload;
    function RunApp(Command: string; NoEchoCmd: boolean): string; overload;
    function RunApp(Command, FileToSave: string): string; overload;
    //Архивирование
    function MakeBz2(DirPath, SavedName: string): string;
    function AppendBz2(DirPath, SavedName: string): string;
    function ExtractBz2(FileName, DirPath: string): string;
    //Создание BlowFish пароля
    function GenBfPassword(PathToKey: string): string;
    //Шифрование BlowFish
    function EncryptFileBf(PathToKeyOrKeyStr, OriginalFile, EncryptedFile:
      string): string;
    function DecryptFileBf(PathToKeyOrKeyStr, OriginalFile, DecryptedFile:
      string): string;
    //Base64
    function Base64Encode(PathToFile, EncodedFile: string): string;
    function Base64Decode(PathToFile, DecodedFile: string): string;
    //Создание RSA ключей
    function GenRsaPrivateKey(PathToKey: string; iSize: integer): string;
    function GenRsaPublicKey(PathToPrivKey, PathToPubKey: string): string;
    function GenRsaKeys(PathToPrivKey, PathToPubKey: string;
      iSize: integer): string;
    //Шифровка и Расшифровка RSA
    function EncryptFile(PathToPubKey, OriginalFile, EncryptedFile: string): string;
    function DecryptFile(PathToPrivKey, CipheredFile, DecodeFile: string): string;
    //Получение почты Pop3
    function Pop3Login(AHost, ALogin, APassword: string): boolean;
    function Pop3Count: integer; // Количество писем
    function GetMail(Index: integer): string; // Получить исходник письма
    function GetCountSection(Mail: string): integer; // Получить количество секций
    function GetSection(Mail: string; Index: integer): string;
    // Получить секцию полностью
    function GetSectionHead(Mail: string; Index: integer): string;
    // Заголовок секции
    function GetSectionBody(Mail: string; Index: integer): string; // Текст секции
    // Сохранение вложения
    function GetSectionFileName(Mail: string; Index: integer): string;
    // Имя файла, если это вложение
    function SaveSectionToFile(FileName: string; Mail: string; Index: integer): boolean;
    // Сохранение в файла
    function SaveSectionToMemoryStream(MemStream: TMemoryStream;
      Mail: string; Index: integer): boolean; // Сохранение в памяти
    // Отправка почты
    function SendMail(pSubject, pTextBody, pHTMLBody: string; Files: TStrings): boolean;
    function KeySend(EMail: string): boolean;      // ???????????
  published
    property HostPop3: string Read fHostPop3 Write fHostPop3;
    property HostSmtp: string Read fHostSmtp Write fHostSmtp;
  end;

implementation

{ TCustomCrypt }

constructor TCustomCrypt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Pop3 := TPOP3Send.Create;
  MailMessage := TMimeMess.Create;
  //UserDB := TUserList.Create (nil);
end;

destructor TCustomCrypt.Destroy;
begin
  Pop3.Free;
  MailMessage.Free;
  inherited Destroy;
end;

function TCustomCrypt.RunApp(Command: string): string;
begin
  Result := RunApp(Command, False);
end;

function TCustomCrypt.RunApp(Command: string; NoEchoCmd: boolean): string;
const
  READ_BYTES = 2048;
var
  S: TStringList;
  M: TMemoryStream;
  P: TProcess;
  n: longint;
  BytesRead: longint;
begin
  M := TMemoryStream.Create;
  BytesRead := 0;
  P := TProcess.Create(nil);
  P.CommandLine := Command;
  {$IFDEF UNIX}
  P.Options := [poUsePipes, poStderrToOutPut];
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  P.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
  {$ENDIF WINDOWS}
  P.Execute;
  while P.Running do
  begin
    Application.ProcessMessages;
    M.SetSize(BytesRead + READ_BYTES);
    n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc(BytesRead, n)
    else
      Sleep(100);
  end;
  repeat
    Application.ProcessMessages;
    M.SetSize(BytesRead + READ_BYTES);
    n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc(BytesRead, n);
  until n <= 0;
  M.SetSize(BytesRead);
  S := TStringList.Create;
  S.LoadFromStream(M);
  if NoEchoCmd then
    Result := S.Text
  else
  begin
    if S.Text <> '' then
      Result := Command + #13 + #10 + #13 + #10 + S.Text
    else
      Result := Command;
  end;
  S.Free;
  P.Free;
  M.Free;
end;

function TCustomCrypt.RunApp(Command, FileToSave: string): string;
const
  READ_BYTES = 2048;
var
  MemoryStream: TMemoryStream;
  Process: TProcess;
  n: longint;
  BytesRead: longint;
begin
  MemoryStream := TMemoryStream.Create;
  BytesRead    := 0;
  Process      := TProcess.Create(nil);
  Process.CommandLine := Command;
  Process.Options := [poUsePipes, poStderrToOutPut];
  Process.Execute;
  while Process.Running do
  begin
    Application.ProcessMessages;
    MemoryStream.SetSize(BytesRead + READ_BYTES);
    n := Process.Output.Read((MemoryStream.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc(BytesRead, n)
    else
      Sleep(10);
  end;
  repeat
    Application.ProcessMessages;
    MemoryStream.SetSize(BytesRead + READ_BYTES);
    n := Process.Output.Read((MemoryStream.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc(BytesRead, n);
  until n <= 0;
  MemoryStream.SetSize(BytesRead);
  Result := Command;
  MemoryStream.Position := 0;
  MemoryStream.SaveToFile(FileToSave);
  Process.Free;
  MemoryStream.Free;
end;

function TCustomCrypt.MakeBz2(DirPath, SavedName: string): string;
begin
  {$IFDEF UNIX}
  Result := RunApp('tar cjf ' + SavedName + ' ' + DirPath);
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := RunApp(ExtractFilePath(ParamStr(0))+'tar.exe -cjf ' + SavedName + ' ' + DirPath);
  {$ENDIF WINDOWS}
end;

function TCustomCrypt.AppendBz2(DirPath, SavedName: string): string;
begin
  {$IFDEF UNIX}
  Result := RunApp('tar rf ' + SavedName + ' ' + DirPath);
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := RunApp(ExtractFilePath(ParamStr(0))+'tar.exe -rf ' + SavedName + ' ' + DirPath);
  {$ENDIF WINDOWS}
end;

function TCustomCrypt.ExtractBz2(FileName, DirPath: string): string;
begin
  {$IFDEF UNIX}
  Result := RunApp('tar xjf ' + FileName + ' -C ' + DirPath);
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := RunApp(ExtractFilePath(ParamStr(0))+'tar.exe -xjf ' + FileName + ' -C ' + DirPath);
  {$ENDIF WINDOWS}
end;

{ TODO : GenBfPassword - Есть предположение, что слишком мелкий ключ }
function TCustomCrypt.GenBfPassword(PathToKey: string): string;
var
  Res:   HResult;
  Uid:   TGuid;
  hFile: TextFile;
begin
  Result := '';
  Res    := CreateGUID(Uid);
  if Res = S_OK then
  begin
    Result := GUIDToString(Uid);
    if PathToKey <> '' then
    begin
      AssignFile(hFile, PathToKey);
      Rewrite(hFile);
      Write(hFile, Result);
      CloseFile(hFile);
    end;
  end;
end;

function TCustomCrypt.EncryptFileBf(PathToKeyOrKeyStr, OriginalFile,
  EncryptedFile: string): string;
var
  szKey: string;
  hFile: TextFile;
begin
  if FileExists(PathToKeyOrKeyStr) then
  begin
    AssignFile(hFile, PathToKeyOrKeyStr);
    Reset(hFile);
    Read(hFile, szKey);
    CloseFile(hFile);
  end
  else
    szKey := PathToKeyOrKeyStr;
  {$IFDEF UNIX}
  Result := RunApp('/usr/bin/openssl bf -a -in ' + OriginalFile +
    ' -out ' + EncryptedFile + ' -pass pass:' + szKey);
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := RunApp(ExtractFilePath(ParamStr(0))+'openssl.exe bf -a -in ' + OriginalFile +
    ' -out ' + EncryptedFile + ' -pass pass:' + szKey);
  {$ENDIF WINDOWS}
end;

function TCustomCrypt.DecryptFileBf(PathToKeyOrKeyStr, OriginalFile,
  DecryptedFile: string): string;
var
  szKey: string;
  hFile: TextFile;
begin
  if FileExists(PathToKeyOrKeyStr) then
  begin
    AssignFile(hFile, PathToKeyOrKeyStr);
    Reset(hFile);
    Read(hFile, szKey);
    CloseFile(hFile);
  end
  else
    szKey := PathToKeyOrKeyStr;
  {$IFDEF UNIX}
  Result := RunApp('/usr/bin/openssl bf -a -d -in ' + OriginalFile +
    ' -out ' + DecryptedFile + ' -pass pass:' + szKey);
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := RunApp(ExtractFilePath(ParamStr(0))+'openssl.exe bf -a -d -in ' + OriginalFile +
    ' -out ' + DecryptedFile + ' -pass pass:' + szKey);
  {$ENDIF WINDOWS}
end;

// Закодировать в Base64
function TCustomCrypt.Base64Encode(PathToFile, EncodedFile: string): string;
{$IFDEF UNIX}
var
  hFile: TextFile;
{$ENDIF UNIX}
begin
  {$IFDEF UNIX}
  Result := trim(RunApp('/usr/bin/base64 ' + PathToFile, True));
  AssignFile(hFile, EncodedFile);
  Rewrite(hFile);
  Write(hFile, Result);
  CloseFile(hFile);
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := trim(RunApp(ExtractFilePath(ParamStr(0))+'base64.exe -e ' +  PathToFile + ' ' + EncodedFile, True));
  {$ENDIF WINDOWS}
end;

// Раскодировать в Base64
function TCustomCrypt.Base64Decode(PathToFile, DecodedFile: string): string;
begin
  {$IFDEF UNIX}
  Result := RunApp('/usr/bin/base64 -d ' + PathToFile, DecodedFile);
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := trim(RunApp(ExtractFilePath(ParamStr(0))+'base64.exe -d ' + PathToFile + ' ' + DecodedFile, True));
  {$ENDIF WINDOWS}
end;

function TCustomCrypt.GenRsaPrivateKey(PathToKey: string; iSize: integer): string;
begin
  // Создание приватного ключа, параметры:
  //          PathToKey - Путь к сохранению сгенирированного ключа, iSize - размер ключа
  {$IFDEF UNIX}
  Result := RunApp('/usr/bin/openssl genrsa -out ' + PathToKey +
    ' ' + IntToStr(iSize));
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := RunApp(ExtractFilePath(ParamStr(0))+'openssl.exe genrsa -out ' + PathToKey +
    ' ' + IntToStr(iSize));
  {$ENDIF WINDOWS}
end;

function TCustomCrypt.GenRsaPublicKey(PathToPrivKey, PathToPubKey: string): string;
begin
  // Создание публичного ключа на основе приватного, параметры:
  //          PathToPrivKey - Путь к уже существующему файлу приватного ключа
  //          PathToPubKey  - Путь включая имя файла к сохранению нового открытого ключа
  {$IFDEF UNIX}
  Result := RunApp('/usr/bin/openssl rsa -in ' + PathToPrivKey +
    ' -pubout -out ' + PathToPubKey);
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := RunApp(ExtractFilePath(ParamStr(0))+'openssl.exe rsa -in ' + PathToPrivKey +
    ' -pubout -out ' + PathToPubKey);
  {$ENDIF WINDOWS}
end;

function TCustomCrypt.GenRsaKeys(PathToPrivKey, PathToPubKey: string;
  iSize: integer): string;
begin
  //Генерация пары ключей
  Result := GenRsaPrivateKey(PathToPrivKey, iSize);
  Result := Result + GenRsaPublicKey(PathToPrivKey, PathToPubKey);
end;

function TCustomCrypt.EncryptFile(PathToPubKey, OriginalFile,
  EncryptedFile: string): string;
begin
  // ШИФРОВАНИЕ RSA, параметры:
  //            PathToPubKey - Путь к чужому открытому ключу
  //            OriginalFile - Путь к оригинальному файлу
  //            EncryptedFile - Путь для сохранения зашифрованного файла
  {$IFDEF UNIX}
  Result := RunApp('/usr/bin/openssl rsautl -inkey ' + PathToPubKey +
    '  -in ' + OriginalFile + ' -out ' + EncryptedFile + ' -pubin -encrypt');
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := RunApp(ExtractFilePath(ParamStr(0))+'openssl.exe rsautl -inkey ' + PathToPubKey +
    '  -in ' + OriginalFile + ' -out ' + EncryptedFile + ' -pubin -encrypt');
  {$ENDIF WINDOWS}
end;

function TCustomCrypt.DecryptFile(PathToPrivKey, CipheredFile,
  DecodeFile: string): string;
begin
  // РАСШИФРОВКА RSA, параметры:
  //             PathToPrivKey - Путь к своему закрытому ключу
  //             CipheredFile  - Путь к зашифрованному файлу
  //             DecodeFile    - Путь и имя файла для записи расшифровки
  {$IFDEF UNIX}
  Result := RunApp('/usr/bin/openssl rsautl -inkey ' + PathToPrivKey +
    ' -in ' + CipheredFile + ' -out ' + DecodeFile + ' -decrypt');
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Result := RunApp(ExtractFilePath(ParamStr(0))+'openssl.exe rsautl -inkey ' + PathToPrivKey +
    ' -in ' + CipheredFile + ' -out ' + DecodeFile + ' -decrypt');
  {$ENDIF WINDOWS}
end;

{ TODO : Нет настроек подключения к почтовому серверу  =( }
function TCustomCrypt.Pop3Login(AHost, ALogin, APassword: string): boolean;
begin
  Application.ProcessMessages;
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
    raise Exception.Create('Соединение не удалось. Проверьте логин/пароль/порт.');
end;

function TCustomCrypt.Pop3Count: integer;
begin
  Application.ProcessMessages;
  if Pop3.Stat then
    Result := Pop3.StatCount
  else
    Result := -1;
end;

function TCustomCrypt.GetMail(Index: integer): string;
begin
  Application.ProcessMessages;
  if Pop3.Retr(Index) then
    Result := Pop3.FullResult.Text
  else
    Result := 'Error';
end;

function TCustomCrypt.GetCountSection(Mail: string): integer;
begin
  Application.ProcessMessages;
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    Result := MessagePart.GetSubPartCount;
  end;
end;

function TCustomCrypt.GetSection(Mail: string; Index: integer): string;
begin
  Application.ProcessMessages;
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := ''
    else
      try
        MessagePart.GetSubPart(Index).PartBody.Text :=
          DecodeBase64(MessagePart.GetSubPart(Index).PartBody.Text);
        Result := MessagePart.GetSubPart(Index).Lines.Text;
      finally
      end;
  end;
end;

function TCustomCrypt.GetSectionHead(Mail: string; Index: integer): string;
begin
  Application.ProcessMessages;
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := ''
    else
      try
        Result := MessagePart.GetSubPart(Index).Headers.Text;
      finally
      end;
  end;
end;

function TCustomCrypt.GetSectionBody(Mail: string; Index: integer): string;
begin
  Application.ProcessMessages;
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := ''
    else
      try
        Result := MessagePart.GetSubPart(Index).PartBody.Text;
      finally
      end;
  end;
end;

function TCustomCrypt.GetSectionFileName(Mail: string; Index: integer): string;
begin
  Application.ProcessMessages;
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := ''
    else
      try
        Result := MessagePart.GetSubPart(Index).FileName;
      finally
      end;
  end;
end;

function TCustomCrypt.SaveSectionToFile(FileName: string; Mail: string;
  Index: integer): boolean;
begin
  Application.ProcessMessages;
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
          MessagePart.GetSubPart(Index).PartBody.Text :=
            DecodeBase64(MessagePart.GetSubPart(Index).PartBody.Text);
          MessagePart.GetSubPart(Index).PartBody.SaveToFile(FileName); // Сохранение
          Result := True;
        end
        else
          Result := False;
      except
        Result := False;
      end;
  end;
end;

function TCustomCrypt.SaveSectionToMemoryStream(MemStream: TMemoryStream;
  Mail: string; Index: integer): boolean;
begin
  Application.ProcessMessages;
  with MailMessage do
  begin
    Lines.Text := Mail;
    DecodeMessage;
    if (Index < 0) or (Index > MessagePart.GetSubPartCount) then
      Result := False
    else
      try
        // Расшифрованная секция
        MessagePart.GetSubPart(Index).PartBody.Text :=
          DecodeBase64(MessagePart.GetSubPart(Index).PartBody.Text);
        MessagePart.GetSubPart(Index).PartBody.SaveToStream(MemStream); // Сохранение
        Result := True;
      except
        Result := False;
      end;
  end;
end;

function TCustomCrypt.SendMail(pSubject, pTextBody, pHTMLBody: string;
  Files: TStrings): boolean;
var
  tmpMsg: TMimeMess;
  tmpStringList: TStringList;
  tmpMIMEPart: TMimePart;
  fItem: string;
begin
  Application.ProcessMessages;
  tmpMsg := TMimeMess.Create;
  tmpStringList := TStringList.Create;
  Result := False;
  try
    // Headers
    tmpMsg.Header.Subject := pSubject;
    tmpMsg.Header.From    := fLogin;
    tmpMsg.Header.ToList.Add(fFriendMail);
    // MIMe Parts
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
    for fItem in Files do
      tmpMsg.AddPartBinaryFromFile(fItem, tmpMIMEPart);
    // кодируем и отправляем
    tmpMsg.EncodeMessage;
    Result := smtpsend.SendToRaw(fLogin, fFriendMail, fHostSmtp,
      tmpMsg.Lines, fLogin, fPassword);
  finally
    tmpMsg.Free;
    tmpStringList.Free;
  end;
end;

{ TODO : Проверить целеособразность процедуры KeySend }
function TCustomCrypt.KeySend(EMail: string): boolean;
var
  Files: TStringList;
begin
  Result := False;
  try
    fFriendMail := EMail;
    Files := TStringList.Create;
    Files.Append('privXID.pem');
    Files.Append('pubXID.pem');
    GenRsaKeys(Files[0], Files[1], 1024);
    SendMail('{86AE072A-941F-408A-BD99-4C2E4845C291}',
      '{65E5DFD8-AED9-4A8F-B2EE-F780373AC3B1}', '',
      Files);
  finally
    Files.Free;
    Result := True;
  end;
end;

end.
