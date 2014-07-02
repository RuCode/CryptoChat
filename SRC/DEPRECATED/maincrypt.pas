unit MainCrypt;

{$mode objfpc}{$H+}

interface

uses
Classes, Controls, CustomCrypt,
Dialogs, ExtCtrls, FileUtil, Forms, Graphics,
md5, Menus, SQLiteWrap, SysUtils;

const

// Названия столбцов таблиц
SQL_COL_ID_USER    = 'ID_USER';
SQL_COL_ID_FRIEND  = 'ID_FRIEND';
SQL_COL_AUTH       = 'AUTH';
SQL_COL_UUID       = 'UUID';
SQL_COL_NICK_NAME  = 'NNAME';
SQL_COL_EMAIL      = 'EMAIL';
SQL_COL_AVATAR     = 'ADATA';
SQL_COL_RESP_DATE  = 'RDATE';
SQL_COL_XID        = 'XID';
SQL_COL_IS_MYMSG   = 'ISMY';
SQL_COL_OPEN_KEY   = 'OPEN';
SQL_COL_SECRET_KEY = 'SECRET';
SQL_COL_BF_KEY     = 'BF';
SQL_COL_ZIP_DATA   = 'ZDATA';
SQL_COL_HASH       = 'HASH';
SQL_COL_MESSAGE    = 'MESSAGE';

// Таблицы
SQL_TBL_USERS      = 'USERS';
SQL_TBL_FRIENDS    = 'FRIENDS';
SQL_TBL_MESSAGES   = 'MESSAGES';

// Сообщения о ошибках
ERROR_SQL_LOGIN    = 'Ошибка аутентификации =(';

type
TUserEntry = record
  // Первичные Ключи
  ID_USER :integer;
  // Ник, Имя, Фамилия, Эл. почта, Хэш SHA1 от пароля и Ава
  NickName,
  Email,
  HashPwd :string;
  Avatar  :TPicture;
end;

TFriendEntry = record
  // Первичные Ключи
  ID_USER, ID_FRIEND :integer;
  Auth   :boolean;
  // У каждого друга свой UUID
  UUID,
  // Ник, Имя, Фамилия, Эл. почта и Ава
  NickName,
  Email  :string;
  Avatar :TPicture;
end;

TMessageEntry = record
  // Первичные Ключи
  ID_USER, ID_FRIEND :integer;
  // Время отправки / получения
  Date    :TDateTime;
  // Порядковый номер сообщения
  XID     :integer;
  // True если отправляли мы
  IsMyMsg :boolean;
  // Криптографические Ключи
  OpenKey,
  SecretKey,
  BFKey,
  // Сообщение
  Message,
  // Файлы
  Files   :string;
end;

type

{ TMainCrypt }

TMainCrypt = class(TCustomCrypt)
{*
Графический компонент
+ Вход в систему (шифровка\расшифровка полей БД при верном пароле)
+ Аутентификация (Запрос аутентификация и отлов подтверждения)
+ Отправка и чтение сообщений
+ Нужно замутить что-то вроде

*}
private
  fLogin        :boolean;    // Залогинились или не?
  fUser         :TUserEntry; // Информация о себе
  fPathDb       :string;
  //* ======================================================================== *//
  SQLDataBase   :TSqliteDatabase;
  SQLTable      :TSqliteTable;
  procedure CreateDataBaseTables;
  //* ======================================================================== *//
  function  GetFriendEntry(Index :integer) :TFriendEntry;
  function  GetUserEntry(Index: Integer): TUserEntry;
  procedure SetFriendEntry(Index :integer; AFriend :TFriendEntry);
public
  function  TextReadFile(FileName :string) :string;
  function  Base64ReadFile(FileName :string) :string;
  procedure CreateAndWrite(FilePath, AData :string);
public
  constructor Create (AOwner   :TComponent); override;
  destructor  Destroy; override;
  //* ======================================================================== *//
  function OpenDB(FileName :string) :boolean;          // Создание базы данных
  function WriteDB(ASQL :string) :boolean;             // Запись в базу данных
  function GetDBFilePath :string;
  function AssignDB :boolean;
  //* ======================================================================== *//
  function GetUserInfo :TUserEntry;                    // Получение данных пользователя
  function AddUser(AUser :TUserEntry) :boolean;        // Добавление нового пользователя
  function GetCountUsers :integer;                     // Количество пользователей
  function ExistUser (AEMail   :string)   :boolean;    // Проверка на наличие
  function LoginUser(AUser :TUserEntry) :boolean; overload; virtual;
  function LoginUser(AEMail, APassword:string) :boolean; overload; virtual;
  function UpdateUser(AUser :TUserEntry) :boolean;
  procedure SetUserImage(AUser :integer; AFileName: String);
  function GetUserImage(AUser :integer; AFileName: String): Boolean;
  property Users[Index :integer]:TUserEntry read GetUserEntry;
  //* ======================================================================== *//
  function AddFriend (AFriend:TFriendEntry)  :boolean; virtual;
  function GetCountFriend :integer;
  function ExistFriend(AEMail :string) :boolean;
  function DeleteFriend(Index :integer) :boolean;
  function UpdateFriend(AFriend :TFriendEntry) :boolean;

  procedure SetFriendImage(AFriendID: integer; AFileName: String);
  function  GetFriendImage(AFriendID: integer; AFileName: String): Boolean;

  property Friend[Index :integer]:TFriendEntry read GetFriendEntry write SetFriendEntry;
    default;
  //* ======================================================================== *//
  function AddMessage(AMsg:TMessageEntry) :boolean;
  function GetCountMessage(AFriendID :integer) :integer;
  function GetMaxXid(AFriendID :integer) :integer;
  function GetMessageEntry(Index, AFriendID :integer) :TMessageEntry;
  // Найти последний открытый ключ товарища
  function GetLastOpenKey(AFriendID :integer) :string;
  // Найти свой последний закрытый ключ
  function GetLastPrivateKey(AFriendID :integer) :string;
  //* ======================================================================== *//
  { TODO : Нужно будет перенести функции из MainCrypt в MailCrypt, а именно MakeFirstMail, ReadFirstMail, MakeMail, ReadMail }
  function MakeFirstMail(AFolder: String; AFriendID :integer): Boolean;
  function ReadFirstMail(AFolder: String; AFriendID :integer): Boolean;
  function MakeMail(AFolder, AText, AFiles :string; AFriendID :integer) :boolean;
  function ReadMail(AFolder:string; AFriendID :integer) :boolean;
  function ReadMail(AFolder:string; AFriendID :integer; ADate: TDateTime) :boolean;
  procedure DeleteCryptFiles(AFolder:string);
published
  property isUserLogin :boolean read fLogin;
end;

implementation

{ TMainCrypt }

procedure TMainCrypt.CreateAndWrite(FilePath, AData :string);
var
  hFile :TextFile;
  szBuf: String;
  ch: Char;
begin
  AssignFile(hFile, FilePath);
  Rewrite(hFile);
  szBuf:= '';
  for ch in AData do
  begin
    if ch = #13 then
      szBuf+= #10
    else
      szBuf+= ch;
  end;
  Write(hFile, szBuf);
  CloseFile(hFile);
end;

function TMainCrypt.Base64ReadFile(FileName :string) :string;
var
  hFile :TextFile;
  Buf   :string;
begin
  Result:= '';
  Base64Encode(FileName, FileName+'.b64');
  AssignFile(hFile, FileName+'.b64');
  Reset(hFile);
  while not EOF(hFile) do
  begin
    ReadLn(hFile, Buf);
    Result:= Result + Buf + #10;
  end;
  CloseFile(hFile);
  DeleteFile(FileName+'.b64');
  Result:= trim(Result);
end;

function TMainCrypt.TextReadFile(FileName :string) :string;
var
  hFile :TextFile;
  Buf   :string;
begin
  Result:= '';
  AssignFile(hFile, FileName);
  Reset(hFile);
  while not EOF(hFile) do
  begin
    ReadLn(hFile, Buf);
    Result:= Result + Buf + #10;
  end;
  CloseFile(hFile);
  Result:= trim(Result);
end;

constructor TMainCrypt.Create(AOwner :TComponent);
begin
  inherited Create(AOwner);
  fPathDb:= '';
  fLogin := False;
end;

function TMainCrypt.OpenDB(FileName :string) :boolean;
begin
  Result:= True;
  if Assigned(SQLDataBase) then
    SQLDataBase.Free;
  try
    SQLDataBase := TSqliteDatabase.Create (FileName);
    fPathDb:= FileName;
    fLogin:= False;
    CreateDataBaseTables;
  except
    Result:= False;
  end;
end;

function TMainCrypt.WriteDB(ASQL :string) :boolean;
begin
  Result:= True;
  try
    SQLDataBase.ExecSQL(ASQL);
  except
    Result:= False;
  end;
end;

function TMainCrypt.GetDBFilePath :string;
begin
  Result:= fPathDb;
end;

function TMainCrypt.AssignDB :boolean;
begin
  Result:= (fPathDb <> '');
end;

function TMainCrypt.GetUserInfo :TUserEntry;
begin
  Result:= fUser;
end;

function TMainCrypt.ExistUser (AEMail   :string)   :boolean;
begin
  Result := False;
  try
    SQLTable := SQLDataBase.GetTable ('SELECT EMAIL FROM USERS');
    try
      while not SQLTable.EOF do
      begin
        if AEMail = SQLTable.FieldAsString (SQLTable.FieldIndex['EMAIL']) then
          Result := True;
        SQLTable.Next;
      end;
    finally
      SQLTable.Free;
    end;
  finally
  end;
end;


function TMainCrypt.GetUserEntry(Index: Integer): TUserEntry;
begin
  Result.ID_USER:= -1;
  try
    SQLTable := SQLDataBase.GetTable ('SELECT * FROM '+SQL_TBL_USERS);
    try
      while not SQLTable.EOF do
      begin
        if (int64(Index)+1) = SQLTable.FieldAsInteger(SQLTable.FieldIndex[SQL_COL_ID_USER]) then
        begin
          Result.ID_USER    :=
            SQLTable.FieldAsInteger(SQLTable.FieldIndex[SQL_COL_ID_USER]);
          Result.NickName   :=
            SQLTable.FieldAsString (SQLTable.FieldIndex[SQL_COL_NICK_NAME]);
          Result.Email      :=
            SQLTable.FieldAsString (SQLTable.FieldIndex[SQL_COL_EMAIL]);
          Result.HashPwd    :=
            SQLTable.FieldAsString (SQLTable.FieldIndex[SQL_COL_HASH]);
        end;
        SQLTable.Next;
      end;
    finally
      SQLTable.Free;
    end;
  finally
  end;
end;

function TMainCrypt.LoginUser(AUser :TUserEntry) :boolean;
begin
  Result := False;
  try
    SQLTable := SQLDataBase.GetTable ('SELECT * FROM '+SQL_TBL_USERS);
    try
      while not SQLTable.EOF do
      begin
        if (AUser.Email = SQLTable.FieldAsString (
          SQLTable.FieldIndex[SQL_COL_EMAIL])) and
          (MD5Print (MD5String (AUser.HashPwd)) =
          SQLTable.FieldAsString (SQLTable.FieldIndex[SQL_COL_HASH])) then
        begin
          fUser.ID_USER    :=
            SQLTable.FieldAsInteger(SQLTable.FieldIndex[SQL_COL_ID_USER]);
          fUser.NickName   :=
            SQLTable.FieldAsString (SQLTable.FieldIndex[SQL_COL_NICK_NAME]);
          fUser.Email      := AUser.Email;
          fUser.HashPwd    := AUser.HashPwd;
          Result           := True;
          fLogin           := True;
        end;
        SQLTable.Next;
      end;
    finally
      SQLTable.Free;
    end;
  finally
  end;
end;

function TMainCrypt.LoginUser(AEMail, APassword :string) :boolean;
var
  TmpUser :TUserEntry;
begin
  TmpUser.Email:= AEMail;
  TmpUser.HashPwd:= APassword;
  Result:= LoginUser(TmpUser);
end;

function TMainCrypt.UpdateUser(AUser :TUserEntry) :boolean;
begin
  // UPDATE USERS SET NNAME = "RCode", FNAME = "Anton" WHERE ID_USER = 2;
  Result:= False;
  try
    SQLTable := SQLDataBase.GetTable ('UPDATE '+SQL_TBL_USERS+' SET ' +
      SQL_COL_EMAIL + ' = "' + AUser.Email + '", ' + SQL_COL_NICK_NAME +
      ' = "' + AUser.NickName +      '"' +      ' WHERE ' +
      SQL_COL_ID_USER+' = "'+IntToStr(fUser.ID_USER)+'";');
    SQLTable.Free;
    if LoginUser(AUser.Email, fUser.HashPwd) then
      Result:= True;
  except
    raise Exception.Create ('Не могу найти пользователя...');
  end;
end;

procedure TMainCrypt.SetUserImage(AUser: integer; AFileName: String);
begin
  WriteDB('UPDATE '+SQL_TBL_USERS+' SET '+SQL_COL_AVATAR+' = "'+Base64ReadFile(AFileName)+
    '" WHERE '+SQL_COL_ID_USER+' = "'+IntToStr(AUser+1)+'";');
end;

function TMainCrypt.GetUserImage(AUser: integer; AFileName: String): Boolean;
var
  hFile: TextFile;
  ch: Char;
  szBuf, szBase: String;
begin
  try
    szBase:= '';
    Result:= True;
    SQLTable := SQLDataBase.GetTable ('SELECT * FROM ' + SQL_TBL_USERS +
             ' WHERE '+SQL_COL_ID_USER+' = "'+IntToStr(AUser+1)+'";');
    try
      AssignFile(hFile, AFileName+'.cr');
      ReWrite(hFile);
      szBase:= SQLTable.FieldAsString(SQLTable.FieldIndex[SQL_COL_AVATAR]);
      if szBase = '' then
      begin
        Result:= False;
        exit;
      end;
      szBuf:= '';
      for ch in szBase do
      begin
        if ch = #13 then
          szBuf+= #10
        else
          szBuf+= ch;
      end;
      Write(hFile, szBuf);
      Close(hFile);
      Base64Decode(AFileName+'.cr', AFileName);
    finally
      SQLTable.Free;
    end;
  finally
    DeleteFile(AFileName+'.cr');
  end;
end;

procedure TMainCrypt.SetFriendImage(AFriendID: integer; AFileName: String);
begin
  WriteDB('UPDATE '+SQL_TBL_FRIENDS+' SET '+SQL_COL_AVATAR+' = "'+Base64ReadFile(AFileName)+
    '" WHERE '+SQL_COL_ID_USER+' = "'+IntToStr(GetUserInfo.ID_USER) +
    '" AND ' + SQL_COL_ID_FRIEND+' = "'+IntToStr(AFriendID+1)+'";');
end;

function TMainCrypt.GetFriendImage(AFriendID: integer; AFileName: String): Boolean;
var
  hFile: TextFile;
  ch: Char;
  szBuf, szBase: String;
begin
  try
    szBase:= '';
    Result:= True;
    SQLTable := SQLDataBase.GetTable ('SELECT * FROM ' + SQL_TBL_FRIENDS +
             ' WHERE '+SQL_COL_ID_USER+' = "'+IntToStr(GetUserInfo.ID_USER) +
             '" AND ' + SQL_COL_ID_FRIEND+' = "'+IntToStr(AFriendID+1)+'";');
    try
      AssignFile(hFile, AFileName+'.cr');
      ReWrite(hFile);
      szBase:= SQLTable.FieldAsString(SQLTable.FieldIndex[SQL_COL_AVATAR]);
      if szBase = '' then
      begin
        Result:= False;
        exit;
      end;
      szBuf:= '';
      for ch in szBase do
      begin
        if ch = #13 then
          szBuf+= #10
        else
          szBuf+= ch;
      end;
      Write(hFile, szBuf);
      Close(hFile);
      Base64Decode(AFileName+'.cr', AFileName);
    finally
      SQLTable.Free;
    end;
  finally
    DeleteFile(AFileName+'.cr');
  end;
end;

function TMainCrypt.AddFriend(AFriend :TFriendEntry) :boolean;
var
  FCount :integer;
  Res  :HResult;
  Uid  :TGuid;
begin
  // INSERT INTO FRIENDS VALUES (1, 2, "86AE072A-941F-408A-BD99-4C2E4845C291", "ANNA", "ANNA", "SINICYANA", "ANNA@MAIL.RU", "");
  Result:= True;
  Res   := CreateGUID (Uid);
  if Res = S_OK then
  begin
    FCount := GetCountFriend()+1;
    WriteDB('INSERT INTO ' + SQL_TBL_FRIENDS+' VALUES ("' + IntToStr (FCount) + '", "' +
      IntToStr(GetUserInfo.ID_USER) + '", ' + '"' + GUIDToString (Uid) +
      '", ' + '"' + AFriend.NickName + '", ' + '"' + AFriend.EMail + '", "");');
  end
  else
    Result:= False;
end;

function TMainCrypt.GetCountFriend :integer;
begin
  Result := 0;
  try
    SQLTable := SQLDataBase.GetTable ('SELECT * FROM '+SQL_TBL_FRIENDS +
      ' WHERE '+ SQL_COL_ID_USER + ' = "' + IntToStr(GetUserInfo.ID_USER)+'";');
    try
      while not SQLTable.EOF do
      begin
        Inc (Result, 1);
        SQLTable.Next;
      end;
    finally
      SQLTable.Free;
    end;
  except
    raise Exception.Create ('Не могу узнать количество Ваших контактов...');
  end;
end;

function TMainCrypt.ExistFriend(AEMail :string) :boolean;
var
  i :integer;
begin
  Result:= False;
  for i:= 0 to GetCountFriend - 1 do
    if GetFriendEntry(i).Email = AEMail then
      Result:= True;
end;

function TMainCrypt.DeleteFriend(Index :integer) :boolean;
var
  FCount, i :integer;
begin
  Result := False;
  try
    FCount:= GetCountFriend;
    if Index > FCount-1 then
      exit;
    if Index < 0 then
      Exit;
    // 1. Удалить друга и сообщения
    WriteDB('DELETE FROM '+SQL_TBL_MESSAGES+' WHERE '+
      SQL_COL_ID_FRIEND+' = "'+IntToStr(Index+1) + '" AND '+
      SQL_COL_ID_USER+' = "'+IntToStr(GetUserInfo.ID_USER)+'";');
    WriteDB('DELETE FROM '+SQL_TBL_FRIENDS+' WHERE '+
      SQL_COL_ID_FRIEND+' = "'+IntToStr(Index+1) + '" AND '+
      SQL_COL_ID_USER+' = "'+IntToStr(GetUserInfo.ID_USER)+'";');
    // 2. Скорректировать индексы
    for I:= Index+2 to FCount do
    begin
      //WriteDB('UPDATE '+SQL_TBL_MESSAGES+' WHERE '+SQL_COL_ID_FRIEND+' = '+IntToStr(I-1));
      WriteDB('UPDATE '+SQL_TBL_FRIENDS+' SET '+SQL_COL_ID_FRIEND+' = '+IntToStr(I-1)+
        ' WHERE '+SQL_COL_ID_FRIEND+' = "'+IntToStr(I)+'";');
      // Надо проверить на сообщениях
      WriteDB('UPDATE '+SQL_TBL_MESSAGES+' SET '+SQL_COL_ID_FRIEND+' = '+IntToStr(I-1)+
        ' WHERE '+SQL_COL_ID_FRIEND+' = "'+IntToStr(I)+'";');
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TMainCrypt.UpdateFriend(AFriend :TFriendEntry) :boolean;
begin
  // UPDATE USERS SET NNAME = "RCode", FNAME = "Anton" WHERE ID_USER = 2;
  // ID Friend меньше на единицу чем в таблице, таким образом от 0 до count - 1
  Result:= False;
  try
    SQLTable := SQLDataBase.GetTable ('UPDATE '+SQL_TBL_FRIENDS+' SET ' +
      SQL_COL_EMAIL + ' = "' + AFriend.Email + '", ' + SQL_COL_NICK_NAME +
      ' = "' +      AFriend.NickName + '"' +       ' WHERE ' +
      SQL_COL_ID_USER +      ' = "' +      IntToStr(GetUserInfo.ID_USER) +
      '" AND ' + SQL_COL_ID_FRIEND + ' = "' + IntToStr(AFriend.ID_FRIEND+1)+'";');
    SQLTable.Free;
    Result:= True;
  except
    raise Exception.Create ('Не могу изменить информацию о друге...');
  end;
end;

function TMainCrypt.AddMessage(AMsg :TMessageEntry) :boolean;
var
  szIsMy :string;
begin
  Result:= True;
  try
    if AMsg.IsMyMsg then
      szIsMy:= 'TRUE'
    else
      szIsMy:= 'FALSE';
    WriteDB('INSERT INTO ' + SQL_TBL_MESSAGES+' VALUES("' +
      IntToStr (AMsg.ID_FRIEND) +  '", "' +      IntToStr (GetUserInfo.ID_USER) +
      '", ' +      '"' + DateTimeToStr(AMsg.Date) + '", ' +
      '"' + IntToStr(AMsg.XID) +
      '", ' + '"' + szIsMy + '", ' + '"' + AMsg.OpenKey +'", ' +
      '"' + AMsg.SecretKey +'", ' + '"' + AMsg.BFKey +  '", ' +
      '"' + AMsg.Message + '", ' +      '"' + AMsg.Files + '");');
  except
    Result:= False;
  end;
end;

function TMainCrypt.GetCountMessage(AFriendID :integer) :integer;
begin
  Result := 0;
  try
    SQLTable := SQLDataBase.GetTable ('SELECT * FROM '+SQL_TBL_MESSAGES +
      ' WHERE '+SQL_COL_ID_FRIEND+' = "'+      IntToStr(AFriendID+1) +
      '" AND '+SQL_COL_ID_USER+' = "'+IntToStr(GetUserInfo.ID_USER)+'";');
    try
      while not SQLTable.EOF do
      begin
        Inc (Result, 1);
        SQLTable.Next;
      end;
    finally
      SQLTable.Free;
    end;
  except
    raise Exception.Create ('Не могу узнать количество сообщений...');
  end;
end;

function TMainCrypt.GetMaxXid(AFriendID :integer) :integer;
var
  aMax :integer;
begin
  Result := 0;
  try
    SQLTable := SQLDataBase.GetTable ('SELECT * FROM '+SQL_TBL_MESSAGES +
      ' WHERE '+SQL_COL_ID_FRIEND+' = "'+ IntToStr(AFriendID+1) +
      '" AND '+SQL_COL_ID_USER+' = "'+IntToStr(GetUserInfo.ID_USER)+'";');
    try
      while not SQLTable.EOF do
      begin
        aMax:= SQLTable.FieldAsInteger(SQLTable.FieldIndex[SQL_COL_XID]);
        if aMax > Result then
          Result:= aMax;
        SQLTable.Next;
      end;
    finally
      SQLTable.Free;
      Result:= aMax;
    end;
  except
    raise Exception.Create ('Не могу узнать XID сообщений...');
  end;
end;

function TMainCrypt.AddUser(AUser :TUserEntry) :boolean;
var
  Count :integer;
begin
  if fPathDb = '' then
  begin
    Result:= False;
    Exit;
  end;
  Result := True;
  try
    Count := GetCountUsers () + 1;
    if not ExistUser (AUser.Email) then
    begin
        WriteDB('INSERT INTO '+SQL_TBL_USERS+' VALUES ("'+IntToStr(Count)+'", "'+
                                    AUser.NickName+'", "'+
                                    AUser.EMail+'", "' +
                                    MD5Print(MD5String (AUser.HashPwd)) + '", "");');

    end
    else
      Result:= False;
  except
    Result := False;
  end;
end;

function TMainCrypt.GetCountUsers :integer;
begin
  Result := 0;
  try
    SQLTable := SQLDataBase.GetTable ('SELECT * FROM '+SQL_TBL_USERS);
    try
      while not SQLTable.EOF do
      begin
        Inc (Result, 1);
        SQLTable.Next;
      end;
    finally
      SQLTable.Free;
    end;
  except
    raise Exception.Create ('Не могу узнать количество пользователей программы...');
  end;
end;

procedure TMainCrypt.CreateDataBaseTables;
begin

  // Таблица users
  SQLDataBase.ExecSQL ('CREATE TABLE IF NOT EXISTS '+SQL_TBL_USERS+'('+
    SQL_COL_ID_USER+' INTEGER, '+SQL_COL_NICK_NAME+', '+
   SQL_COL_EMAIL+', '+SQL_COL_HASH+
    ' TEXT, '+SQL_COL_AVATAR+' BLOB);');

  // Таблица friends
  SQLDataBase.ExecSQL ('CREATE TABLE IF NOT EXISTS '+SQL_TBL_FRIENDS+
    '('+SQL_COL_ID_FRIEND+', '+    SQL_COL_ID_USER+' INTEGER, '+SQL_COL_UUID+', ' +
    SQL_COL_NICK_NAME +    ', '+SQL_COL_EMAIL+' TEXT, '+SQL_COL_AVATAR+' BLOB);');

  // Таблица messages
  SQLDataBase.ExecSQL ('CREATE TABLE IF NOT EXISTS ' + SQL_TBL_MESSAGES +
    '('+SQL_COL_ID_FRIEND+' INTEGER,' + SQL_COL_ID_USER+' INTEGER, '+
    SQL_COL_RESP_DATE+' TEXT, '+SQL_COL_XID+' INTEGER, '+
    SQL_COL_IS_MYMSG+', '+SQL_COL_OPEN_KEY+', '+SQL_COL_SECRET_KEY+
    ','+SQL_COL_BF_KEY+',' +    SQL_COL_MESSAGE+', '+SQL_COL_ZIP_DATA+
    ' TEXT);');
{
  // Таблица users
  SQLDataBase.ExecSQL ('CREATE TABLE IF NOT EXISTS '+SQL_TBL_USERS+'('+
    SQL_COL_ID_USER+' INTEGER, '+SQL_COL_NICK_NAME+', '+
   SQL_COL_EMAIL+', '+SQL_COL_HASH+
    ' TEXT, '+SQL_COL_AVATAR+' BLOB, PRIMARY KEY('+SQL_COL_ID_USER+'));');

  // Таблица friends
  SQLDataBase.ExecSQL ('CREATE TABLE IF NOT EXISTS '+SQL_TBL_FRIENDS+
    '('+SQL_COL_ID_FRIEND+', '+    SQL_COL_ID_USER+' INTEGER, '+SQL_COL_UUID+', ' +
    SQL_COL_NICK_NAME +    ', '+SQL_COL_EMAIL+' TEXT, '+SQL_COL_AVATAR+' BLOB, PRIMARY KEY('+
    SQL_COL_ID_FRIEND+'), FOREIGN KEY ('    +    SQL_COL_ID_USER+
    ') REFERENCES '+SQL_TBL_USERS+'('+SQL_COL_ID_USER+'));');

  // Таблица messages
  SQLDataBase.ExecSQL ('CREATE TABLE IF NOT EXISTS ' + SQL_TBL_MESSAGES +
    '('+SQL_COL_ID_FRIEND+' INTEGER,' + SQL_COL_ID_USER+' INTEGER, '+
    SQL_COL_RESP_DATE+' TEXT, '+SQL_COL_XID+' INTEGER, '+
    SQL_COL_IS_MYMSG+', '+SQL_COL_OPEN_KEY+', '+SQL_COL_SECRET_KEY+
    ','+SQL_COL_BF_KEY+',' +    SQL_COL_MESSAGE+', '+SQL_COL_ZIP_DATA+
    ' TEXT, FOREIGN KEY ('+SQL_COL_ID_FRIEND+
    ') REFERENCES FRIENDS('+SQL_COL_ID_FRIEND+'));');
}
end;

function TMainCrypt.GetMessageEntry(Index, AFriendID :integer) :TMessageEntry;
var
  szIsMy  :string;
  CurrInd :integer;
begin
  Result.ID_FRIEND:= -1;
  try
    if (Index < 0) or (Index > GetCountMessage(AFriendID)) then
      exit;
    if (AFriendID < 0) or (AFriendID > GetCountFriend) then
      exit;
    SQLTable := SQLDataBase.GetTable ('SELECT * FROM '+SQL_TBL_MESSAGES+
      ' WHERE '+SQL_COL_ID_FRIEND+' = "'+      IntToStr(AFriendID+1) +
      '" AND '+SQL_COL_ID_USER+' = "'+IntToStr(GetUserInfo.ID_USER)+'";');
    try
      CurrInd:= -1;
      while not SQLTable.EOF do
      begin
        Inc(CurrInd, 1);
        if CurrInd = Index then
        begin
          Result.ID_FRIEND:= SQLTable.FieldAsInteger(
            SQLTable.FieldIndex[SQL_COL_ID_FRIEND]);
          Result.ID_USER  := SQLTable.FieldAsInteger(
            SQLTable.FieldIndex[SQL_COL_ID_USER]);
          Result.Date     := StrToDateTime(SQLTable.FieldAsString (
            SQLTable.FieldIndex[SQL_COL_RESP_DATE]));
          Result.XID      := SQLTable.FieldAsInteger (
            SQLTable.FieldIndex[SQL_COL_XID]);
          szIsMy          := SQLTable.FieldAsString (
            SQLTable.FieldIndex[SQL_COL_IS_MYMSG]);
          Result.IsMyMsg  := (szIsMy = 'TRUE');
          Result.Message  := SQLTable.FieldAsString (
            SQLTable.FieldIndex[SQL_COL_MESSAGE]);
          Result.Files    := SQLTable.FieldAsString (
            SQLTable.FieldIndex[SQL_COL_ZIP_DATA]);
          Result.OpenKey  := SQLTable.FieldAsString (
            SQLTable.FieldIndex[SQL_COL_OPEN_KEY]);
          Result.SecretKey:= SQLTable.FieldAsString (
            SQLTable.FieldIndex[SQL_COL_SECRET_KEY]);
          Result.BFKey    := SQLTable.FieldAsString (
            SQLTable.FieldIndex[SQL_COL_BF_KEY]);
        end;
        SQLTable.Next;
      end;
    finally
      SQLTable.Free;
    end;
  except
    raise Exception.Create ('Не могу найти сообщение...');
  end;
end;

function TMainCrypt.GetLastOpenKey(AFriendID :integer) :string;
var
  i, Count :integer;
  Msg      :TMessageEntry;
begin
  Result:= '';
  Count := GetCountMessage(AFriendID);
  for i:= Count downto 0 do
  begin
    Msg:= GetMessageEntry(i, AFriendID);
    if not Msg.IsMyMsg then
      Result:= trim(Msg.OpenKey);
  end;
end;

function TMainCrypt.GetLastPrivateKey(AFriendID :integer) :string;
var
  i, Count :integer;
  Msg      :TMessageEntry;
begin
  Result:= '';
  Count := GetCountMessage(AFriendID);
  for i:= Count downto 0 do
  begin
    Msg:= GetMessageEntry(i, AFriendID);
    if Msg.IsMyMsg then
      Result:= trim(Msg.SecretKey);
  end;
end;

function TMainCrypt.MakeFirstMail(AFolder: String; AFriendID :integer): Boolean;
var
  AMsg :TMessageEntry;
begin
  Result:= False;
  try
    // Создаём ключи
    GenRsaKeys(AFolder+'Priv.key', AFolder+'Pub.key', 2048);
    GenBfPassword(AFolder+'BFKEY.TXT');
    // Так то ещё нужно в БД добавить и отправить почтой =(
    AMsg.ID_FRIEND:= AFriendID+1;
    AMsg.ID_USER  := GetUserInfo.ID_USER;
    AMsg.Date     := Now;
    AMsg.IsMyMsg  := True;
    AMsg.XID      := 1;
    AMsg.Message  := '';
    AMsg.OpenKey  := TextReadFile(AFolder+'Pub.key');
    AMsg.SecretKey:= Base64ReadFile(AFolder+'Priv.key');
    AMsg.BFKey    := TextReadFile(AFolder+'BFKEY.TXT');
    AMsg.Files    := '';
    AddMessage(AMsg);
    if MessageDlg('Вопрос', 'Удалить файлы?', mtConfirmation, [mbOk, mbCancel], 0) = mrOK then
    begin
        DeleteFile(AFolder+'Pub.key');
        DeleteFile(AFolder+'Priv.key');
        DeleteFile(AFolder+'Priv.key.b64');
        DeleteFile(AFolder+'BFKEY.TXT');
    end;
  finally
    Result:= True;
  end;
end;

function TMainCrypt.ReadFirstMail(AFolder: String; AFriendID: integer): Boolean;
var
  AMsg :TMessageEntry;
begin
  Result:= False;
  try
    // Так то ещё нужно в БД добавить и отправить почтой =(
    AMsg.ID_FRIEND:= AFriendID+1;
    AMsg.ID_USER  := GetUserInfo.ID_USER;
    AMsg.Date     := Now;
    AMsg.IsMyMsg  := FALSE;
    AMsg.XID      := 1;
    AMsg.Message  := '';
    AMsg.OpenKey  := TextReadFile(AFolder+'Pub.key');
    AMsg.SecretKey:= '';
    AMsg.BFKey    := TextReadFile(AFolder+'BFKEY.TXT');
    AMsg.Files    := '';
    AddMessage(AMsg);
    if MessageDlg('Вопрос', 'Удалить файлы?', mtConfirmation, [mbOk, mbCancel], 0) = mrOK then
    begin
        DeleteFile(AFolder+'Pub.key');
        DeleteFile(AFolder+'BFKEY.TXT');
    end;
  finally
    Result:= True;
  end;
end;

function TMainCrypt.MakeMail(AFolder, AText, AFiles :string;AFriendID :integer) :boolean;
var
  StringList :TStringList;
  i    :integer;
  AMsg :TMessageEntry;
begin
  Result:= False;
  try
    // Создаём ключи
    GenRsaKeys(AFolder+'Priv.key', AFolder+'Pub.key', 2048);
    GenBfPassword(AFolder+'BFKEY.TXT');
    CreateAndWrite(AFolder+'friend.pem', GetLastOpenKey(AFriendID));
    // Подготавливаем файлы
    CreateAndWrite(AFolder+'MSG.TXT', AText);
    StringList     := TStringList.Create;
    StringList.Text:= AFiles;
    for i:= 0 to StringList.Count - 1 do
    begin
      MakeBz2(StringList[i], AFolder+'FILES.bz2');
      //ShowMessage('StringList: ' + StringList[i]);
      //ShowMessage('Path to BZ: ' + AFolder+'FILES.bz2');
    end;
    // Шифруем все файлы BlowFish, в том числе и свой новый публичный ключ
    EncryptFileBf(AFolder+'BFKEY.TXT', AFolder+'FILES.bz2', AFolder+'FILES.enc');
    EncryptFileBf(AFolder+'BFKEY.TXT', AFolder+'MSG.TXT', AFolder+'MSG.enc');
    EncryptFileBf(AFolder+'BFKEY.TXT', AFolder+'Pub.key', AFolder+'Pub.enc');
    // Нужно получить последний открытый ключ от друга
    //       из истории переписки и им зашифровать ключ BlowFish
    EncryptFile(AFolder+'friend.pem', AFolder+'BFKEY.TXT', AFolder+'BFKEY.enc');
    // Так то ещё нужно в БД добавить и отправить почтой =(
    AMsg.ID_FRIEND:= AFriendID+1;
    AMsg.ID_USER  := GetUserInfo.ID_USER;
    AMsg.Date     := Now;
    AMsg.IsMyMsg  := True;
    AMsg.XID      := GetMaxXid(AFriendID) + 1;
    AMsg.Message  := TextReadFile(AFolder+'MSG.TXT');
    AMsg.OpenKey  := TextReadFile(AFolder+'Pub.key');
    AMsg.SecretKey:= Base64ReadFile(AFolder+'Priv.key');
    AMsg.BFKey    := TextReadFile(AFolder+'BFKEY.TXT');
    AMsg.Files    := Base64ReadFile(AFolder+'FILES.bz2');
    AddMessage(AMsg);
    // Чистим за собой
    //ShowMessage('Сча всё удалю');
    StringList.Free;
    //if MessageDlg('Вопрос', 'Удалить файлы?', mtConfirmation, [mbOk, mbCancel], 0) = mrOK then
    begin
         DeleteFile(AFolder+'FILES.bz2.b64');
         DeleteFile(AFolder+'MSG.TXT.b64');
         DeleteFile(AFolder+'Pub.key.b64');
         DeleteFile(AFolder+'Priv.key.b64');
         DeleteFile(AFolder+'FILES.bz2');
         DeleteFile(AFolder+'MSG.TXT');
         DeleteFile(AFolder+'Pub.key');
         DeleteFile(AFolder+'Priv.key');
         DeleteFile(AFolder+'friend.pem');
         DeleteFile(AFolder+'BFKEY.TXT');
    end;
  finally
    Result:= True;
  end;
end;

function TMainCrypt.ReadMail(AFolder :string; AFriendID :integer) :boolean;
begin
  Result:= ReadMail(AFolder, AFriendID, Now);
end;

function TMainCrypt.ReadMail(AFolder: string; AFriendID: integer;
  ADate: TDateTime): boolean;
var
  AMsg :TMessageEntry;
begin
  Result:= False;
  try
    // Создаём ключи
    CreateAndWrite(AFolder+'MyPrivate.KEY', GetLastPrivateKey(AFriendID));
    Base64Decode(AFolder+'MyPrivate.KEY', AFolder+'Private.KEY');
    DecryptFile(AFolder+'Private.KEY', AFolder+'BFKEY.enc', AFolder+'BFKEY.TXT');
    DecryptFileBf(AFolder+'BFKEY.TXT', AFolder+'FILES.enc', AFolder+'FILES.bz2');
    DecryptFileBf(AFolder+'BFKEY.TXT', AFolder+'MSG.enc', AFolder+'MSG.TXT');
    DecryptFileBf(AFolder+'BFKEY.TXT', AFolder+'Pub.enc', AFolder+'Pub.key');
    // Так то ещё нужно в БД добавить и отправить почтой =(
    AMsg.ID_FRIEND:= AFriendID+1;
    AMsg.ID_USER  := GetUserInfo.ID_USER;
    AMsg.Date     := ADate;
    AMsg.IsMyMsg  := False;
    AMsg.XID      := GetMaxXid(AFriendID);
    AMsg.Message  := TextReadFile(AFolder+'MSG.TXT');
    AMsg.OpenKey  := TextReadFile(AFolder+'Pub.key');
    AMsg.SecretKey:= '';
    AMsg.BFKey    := TextReadFile(AFolder+'BFKEY.TXT');
    AMsg.Files    := Base64ReadFile(AFolder+'FILES.bz2');
    AddMessage(AMsg);
    // Чистим за собой
    //ShowMessage('Сча всё удалю');
  finally
    //Exception.Create('Чтение файлов - не завершено');
    Result:= True;
  end;
end;

procedure TMainCrypt.DeleteCryptFiles(AFolder: string);
begin
  DeleteFile(AFolder+'FILES.bz2.b64');
  DeleteFile(AFolder+'MSG.TXT.b64');
  DeleteFile(AFolder+'Pub.key.b64');
  DeleteFile(AFolder+'Priv.key.b64');
  DeleteFile(AFolder+'FILES.bz2');
  DeleteFile(AFolder+'MSG.TXT');
  DeleteFile(AFolder+'Pub.key');
  DeleteFile(AFolder+'Priv.key');
  DeleteFile(AFolder+'FriendPub.KEY');
  DeleteFile(AFolder+'BFKEY.TXT');
end;

function TMainCrypt.GetFriendEntry(Index :integer) :TFriendEntry;
begin
  Result.ID_FRIEND:= -1;
  try
    if (Index < 0) or (Index > GetCountFriend) then
      exit;
    SQLTable := SQLDataBase.GetTable ('SELECT * FROM '+SQL_TBL_FRIENDS+
      ' WHERE '+SQL_COL_ID_FRIEND+' = "'+      IntToStr(Index+1) +
      '" AND '+SQL_COL_ID_USER+' = "'+IntToStr(GetUserInfo.ID_USER)+'";');
    try
      while not SQLTable.EOF do
      begin
        Result.ID_USER  := SQLTable.FieldAsInteger(
          SQLTable.FieldIndex[SQL_COL_ID_USER]);
        Result.ID_FRIEND:= SQLTable.FieldAsInteger(
          SQLTable.FieldIndex[SQL_COL_ID_FRIEND]);
        Result.Email    := SQLTable.FieldAsString ( SQLTable.FieldIndex[SQL_COL_EMAIL] );

        Result.NickName := SQLTable.FieldAsString (
          SQLTable.FieldIndex[SQL_COL_NICK_NAME]);
        Result.UUID     := SQLTable.FieldAsString ( SQLTable.FieldIndex[SQL_COL_UUID] );
        SQLTable.Next;
      end;
    finally
      SQLTable.Free;
    end;
  except
    raise Exception.Create ('Не могу найти пользователя...');
  end;
end;

procedure TMainCrypt.SetFriendEntry(Index :integer; AFriend :TFriendEntry);
begin
  // UPDATE USERS SET NNAME = "RCode", FNAME = "Anton" WHERE ID_USER = 2;
  try
    SQLTable := SQLDataBase.GetTable ('UPDATE '+SQL_TBL_FRIENDS+' SET ' +
      SQL_COL_EMAIL + ' = "' + AFriend.Email + '", ' + SQL_COL_NICK_NAME +      ' = "' +
      AFriend.NickName + '", ' + ' WHERE '+SQL_COL_ID_FRIEND+' = "'+
      IntToStr(Index+1) +      '" AND '+SQL_COL_ID_USER+' = "'+IntToStr(fUser.ID_USER)+'";');
    SQLTable.Free;
  except
    raise Exception.Create ('Не могу найти пользователя...');
  end;
end;

destructor TMainCrypt.Destroy;
begin
  inherited Destroy;
end;

end.
