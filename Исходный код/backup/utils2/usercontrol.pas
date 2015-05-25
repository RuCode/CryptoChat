unit UserControl;

{/*

     Модуль UserControl реализует интерфейс контроля в многопользовательской среде.

*/}

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, Process, pop3send, smtpsend, mimepart, mimemess,
mimeinln, SynaCode, Dialogs, SQLiteWrap, md5;

type
 TFriendEntry = record
   ID: Integer;
   UUID,
   NickName,
   FirstName,
   LastName,
   Email: String;
 end;

type

{ TUserList }

TUserList = class(TComponent)
private
  fAuth: Boolean;
  fUserId: Integer;
  fNickName, fFirstName, fLastName, fEmail, fPassword   :string;
  database   :TSqliteDatabase;
  tab        :TSqliteTable;
  procedure CreateUserTables;
public
  constructor Create (AOwner   :TComponent); override;
  destructor  Destroy; override;

  function RegisterUser (ANickName, AFirstName, ALastName, AEMail, APassword   :string)
     :boolean;
  function ExistUser (AEMail   :string)   :boolean;
  function LoginUser (AEMail, APassword   :string)   :boolean;
  function GetCountUsers ()   :integer;
  //function GetFriend(ID: Index): TFriendEntry;
  function GetUsersList(): String;

  function GetFriendList(): String;

  function GetFriend(Index: Integer): TFriendEntry;
  function GetFriendCount: Integer;

  procedure AddContact (ANickName, AFirstName, ALastName, AEMail, AImgPath:string);
  procedure AddMessage;

published
  property IsAuthorized: Boolean read fAuth;
  property UserId: Integer read FUserId;
  property NickName: String read fNickName write fNickName;
  property FirstName: String read fFirstName write fFirstName;
  property LastName: String read fLastName write fLastName;
  property EMail: String read fEMail write fEMail;
  property Password: String read fPassword write fPassword;
end;

implementation


{ TUserList }

constructor TUserList.Create (AOwner   :TComponent);
var
  s :string;
begin
  inherited Create (AOwner);
  fAuth:= false;
  database := TSqliteDatabase.Create ('crypto.db3');
  CreateUserTables;
end;

////////////////////////////////////////////////////////////////////////////////

// РЕГИСТРАЦИЯ НОВОГО ПОЛЬЗОВАТЕЛЯ

////////////////////////////////////////////////////////////////////////////////
function TUserList.RegisterUser (ANickName, AFirstName, ALastName, AEMail,
APassword   :string)   :boolean;
var
  sql   :string;
  Count :integer;
begin
  Result := True;
  try
    // Если основные таблицы не созданы, создаём
    CreateUserTables;
    // Узнаём количество пользователей
    try
      Count := GetCountUsers () + 1;
    except
      Count := 1;
    end;
    // Проверяем есть ли уже такой пользователь
    if not ExistUser (AEMail) then
    begin
      // Создаём пользователя
      ShowMessage ('No user exsist');
      sql := 'INSERT INTO USERS(ID_USER, NICK_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH) VALUES(';
      sql += IntToStr (GetCountUsers () + 1) + ', ' + IntToStr(UserId) + ', ';
      sql += '"' + ANickName + '", ';
      sql += '"' + AFirstName + '", ';
      sql += '"' + ALastName + '", ';
      sql += '"' + AEMail + '", ';
      sql += '"' + MD5Print (MD5String (APassword)) + '");';
      database.ExecSQL (sql);
    end;
  except
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// ЕСТЬ ЛИ ПОЛЬЗОВАТЕЛЬ С ТАКИМ ЭЛЕКТРОННЫМ АДРЕССОМ???

////////////////////////////////////////////////////////////////////////////////
function TUserList.ExistUser (AEMail   :string)   :boolean;
begin
  Result := False;
  try
    //database.AddParamInt (':ID_USER', 1);
    tab := database.GetTable ('SELECT EMAIL FROM USERS');
    try
      while not tab.EOF do
      begin
        if AEMail = tab.FieldAsString (tab.FieldIndex['EMAIL']) then
          Result := True;
        tab.Next;
      end;
    finally
      tab.Free;
    end;
  finally
  end;
end;


////////////////////////////////////////////////////////////////////////////////

// АВТОРИЗАЦИЯ ПОЛЬЗОВАТЕЛЯ

////////////////////////////////////////////////////////////////////////////////
function TUserList.LoginUser (AEMail, APassword   :string)   :boolean;
begin
  Result := False;
  try
    tab := database.GetTable ('SELECT * FROM USERS');
    try
      while not tab.EOF do
      begin
        if (AEMail = tab.FieldAsString (tab.FieldIndex['EMAIL'])) and
          (MD5Print (MD5String (APassword)) = tab.FieldAsString (
          tab.FieldIndex['HASH'])) then
        begin
          fUserID   := tab.FieldAsInteger(tab.FieldIndex['ID_USER']);
          NickName  := tab.FieldAsString (tab.FieldIndex['NICK_NAME']);
          FirstName := tab.FieldAsString (tab.FieldIndex['FIRST_NAME']);
          LastName  := tab.FieldAsString (tab.FieldIndex['LAST_NAME']);
          Email     := AEMail;
          Password  := APassword;
          Result     := True;
          fAuth      := True;
        end;
        tab.Next;
      end;
    finally
      tab.Free;
    end;
  finally
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// КОЛИЧЕСТВО ПОЛЬЗОВАТЕЛЕЙ ПРОГРАММЫ

////////////////////////////////////////////////////////////////////////////////
function TUserList.GetCountUsers   :integer;
begin
  Result := 0;
  try
    tab := database.GetTable ('SELECT * FROM USERS');
    try
      while not tab.EOF do
      begin
        Inc (Result, 1);
        tab.Next;
      end;
    finally
      tab.Free;
    end;
  except
    raise Exception.Create ('Не могу узнать количество пользователей программы...');
  end;
end;

function TUserList.GetFriend(Index: Integer): TFriendEntry;
begin
  Result.ID:= -1;
  try
    tab := database.GetTable ('SELECT * FROM FRIENDS WHERE ID_FRIEND = '+ IntToStr(Index+1) +
    ' AND ID_USER = '+IntToStr(fUserId));      // fUserId
    try
      while not tab.EOF do
      begin
        Result.ID:= tab.FieldAsInteger (tab.FieldIndex['ID_FRIEND']);
           Result.Email:= tab.FieldAsString (tab.FieldIndex['EMAIL']);
           Result.FirstName:= tab.FieldAsString (tab.FieldIndex['FIRST_NAME']);
           Result.LastName:= tab.FieldAsString (tab.FieldIndex['LAST_NAME']);
           Result.NickName:= tab.FieldAsString (tab.FieldIndex['NICK_NAME']);
           Result.UUID:= tab.FieldAsString (tab.FieldIndex['UUID']);
           tab.Next;
      end;
    finally
      tab.Free;
    end;
  except
    raise Exception.Create ('Не могу найти пользователя...');
  end;
end;

function TUserList.GetFriendCount: Integer;
begin
  Result := -1;
  try
    tab := database.GetTable ('SELECT * FROM FRIENDS WHERE ID_USER = '+IntToStr(fUserId));
    try
      while not tab.EOF do
      begin
        Result+= 1;
        tab.Next;
      end;
    finally
      tab.Free;
    end;
  except
    raise Exception.Create ('Не могу найти пользователя...');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// ПОЛУЧИТЬ СПИСОК ПОЛЬЗОВАТЕЛЕЙ ПРОГРАММЫ

////////////////////////////////////////////////////////////////////////////////
function TUserList.GetUsersList: String;
begin
  Result := '';
  try
    tab := database.GetTable ('SELECT * FROM USERS');
    try
      while not tab.EOF do
      begin
           Result+= tab.FieldAsString (tab.FieldIndex['EMAIL']) + #13 + #10;
           tab.Next;
      end;
    finally
      tab.Free;
    end;
  except
    raise Exception.Create ('Не могу узнать список email адресов пользователей программы...');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// ПОЛУЧИТЬ СПИСОК ДРУЗЕЙ

////////////////////////////////////////////////////////////////////////////////
function TUserList.GetFriendList: String;
begin
  Result := '';
  try
    tab := database.GetTable ('SELECT * FROM FRIENDS');
    try
      while not tab.EOF do
      begin
           Result+= tab.FieldAsString (tab.FieldIndex['NICK_NAME']) + ' < ';
           Result+= tab.FieldAsString (tab.FieldIndex['EMAIL']) + ' >' + #13 + #10;
           tab.Next;
      end;
    finally
      tab.Free;
    end;
  except
    raise Exception.Create ('Не могу узнать список email адресов пользователей программы...');
  end;
end;

procedure TUserList.AddContact(ANickName, AFirstName, ALastName, AEMail, AImgPath:string);
var
  i    :integer;
  fSQL :string;
  Res  :HResult;
  Uid  :TGuid;
begin
  i   := 0;
  Res := CreateGUID (Uid);
  if Res = S_OK then
  begin
    try
      tab := database.GetTable ('SELECT * FROM FRIENDS');
      try
        while not tab.EOF do
        begin
          Inc (i, 1);
          tab.Next;
        end;
      finally
        tab.Free;
      end;
    finally
    end;
    fSQL := 'INSERT INTO FRIENDS VALUES (' + IntToStr (i + 1) + ', ' +IntToStr (UserId) +
      ', "' + GUIDToString (Uid) + '", "' + ANickName + '", "' +
      AFirstName + '", "' + ALastName + '", "' + AEMail + '", "");';
    //ShowMessage(fSQL);
    database.ExecSQL (fSQl);
  end
  else
    raise Exception.Create ('Не могу добавить вашего друга в контакт лист...');
end;

procedure TUserList.AddMessage;
begin

end;

////////////////////////////////////////////////////////////////////////////////

// СОЗДАНИЕ ОСНОВНЫХ ТАБЛИЦ

////////////////////////////////////////////////////////////////////////////////
procedure TUserList.CreateUserTables;
begin
  // Таблица users
  database.ExecSQL ('CREATE TABLE IF NOT EXISTS USERS(ID_USER INTEGER, NICK_NAME, FIRST_NAME, '
    + 'LAST_NAME, EMAIL, HASH TEXT, AVATAR BLOB, PRIMARY KEY(ID_USER));');
  // Таблица friends
  database.ExecSQL ('CREATE TABLE IF NOT EXISTS FRIENDS(ID_FRIEND, ID_USER INTEGER, UUID, ' +
    'NICK_NAME, FIRST_NAME, LAST_NAME, EMAIL TEXT, AVATAR BLOB, PRIMARY KEY(ID_FRIEND), FOREIGN KEY (ID_USER) REFERENCES USERS(ID_USER));');
  // Таблица messages
  database.ExecSQL ('CREATE TABLE IF NOT EXISTS MESSAGES(ID_FRIEND INTEGER,' +
    'RDATE DATE, RTIME TIME, XID INTEGER, ISMYMSG BOOLEAN, OPEN_KEY BLOB, SECRET_KEY BLOB,'+
    'MESSAGE BLOB, ZIP_FILES, FOREIGN KEY (ID_FRIEND) REFERENCES FRIENDS(ID));');
end;

destructor TUserList.Destroy;
begin
  database.Free;
  inherited Destroy;
end;


end.
