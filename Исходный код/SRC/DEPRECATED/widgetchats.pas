unit WidgetChats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CryptChat, MainCrypt, Dialogs, Forms, Unix, ExtCtrls;

const
  // Сообщения всегда имеют эту тему
  CHAT_MAIN_SUBJECT     = '{6D78D26F-347E-4D57-9E9C-03C82139CD38}';
  CHAT_MAIN_ADD_CONTACT = '{138C7A7C-8F74-4DAF-838B-21E6842A031D}';
  CHAT_INOUT_MESSAGE    = '{DDDAAC7D-DF47-4588-B596-FF963B6B0328}';

type

  { TCryptChat }

  TCryptChat = class(TCustomCryptChat)
  private
    fLastCountMails: Integer;
    UpdateTimer: TTimer;
  protected
    procedure MainNewContact(MailIndex: integer);
    procedure MainReadMessage(MailIndex: integer);
    procedure OnTimerUpdate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GlobalUpdate; override;
    procedure Send(AFriendID: integer; AText: string; ADate: TDateTime; AFiles: TFiles); override;
    procedure ContactDblClick(Sender: TObject); override;
  public
    function GetFriendIndex(AMail: String): Integer;
    function ExistMessage(FriendIndex: Integer; ADate: TDateTime): Boolean;
    function GetNickFromMail(AMail: String): String;
    function LoadOpenKey(FriendIndex, MailIndex: Integer): Boolean;
  published
    property HostPop3;
    property HostSmtp;
    property Host;
  end;

implementation

{ TCryptChat }

function TCryptChat.GetFriendIndex(AMail: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i := 0 to GetCountFriend - 1 do
    if SameText(Friend[i].Email, AMail) then
       Result:= i;
end;

function TCryptChat.ExistMessage(FriendIndex: Integer; ADate: TDateTime): Boolean;
var
  i: Integer;
begin
  Result:= false;
  for i:= 0 to GetCountMessage(FriendIndex)-1 do
    if GetMessageEntry(i, FriendIndex).Date = ADate then
    begin
       Result:= True;
       break;
    end;
end;

function TCryptChat.GetNickFromMail(AMail: String): String;
var
  ch: Char;
begin
  Result:= '';
  for ch in AMail do
    if ch = '@' then
      break
    else
      Result+= ch;
end;

function TCryptChat.LoadOpenKey(FriendIndex, MailIndex: Integer): Boolean;
var
  Msg: TMessageEntry;
begin
  Msg.ID_FRIEND:= FriendIndex;
  Msg.ID_USER:= GetUserInfo.ID_USER;
  msg.Date:= GetMailDate(MailIndex);
  Msg.IsMyMsg:= false;
  Msg.XID:= GetMaxXid(FriendIndex-1);
  SaveAttachToFile(MailIndex, 0, '/tmp/public.key');
  Msg.OpenKey:= TextReadFile('/tmp/public.key');
  Result:= AddMessage(Msg);
end;

procedure TCryptChat.MainNewContact(MailIndex: integer);
const
  mrNone = 0;
  mrYes = mrNone + 6;
var
  FriendIndex: Integer;
  FriendEntry: TFriendEntry;
  isLoadMessage: Boolean;
begin
  // Если наше письмо, то выходим
  if SameText(GetUserInfo.Email, GetMailFrom(MailIndex)) then
    exit;
  // Узнаём индекс друга
  FriendIndex:= GetFriendIndex(GetMailFrom(MailIndex));
  if FriendIndex = -1 then
  begin
    // Такого друга нет
    if MessageDlg('Вопрос', 'К Вам хочет добавиться в друзья: '+GetMailFrom(MailIndex)+', добавить?',
       mtConfirmation, [mbYes, mbNo], 0) = MrYes then
    begin
      FriendEntry.ID_USER:= GetUserInfo.ID_USER;
      FriendEntry.ID_FRIEND:= GetCountFriend + 1;
      if FriendEntry.ID_FRIEND = 0 then
         FriendEntry.ID_FRIEND:= 1;
      FriendEntry.Email:= GetMailFrom(MailIndex);
      FriendEntry.NickName:= GetNickFromMail(FriendEntry.Email);
      if AddFriend(FriendEntry) then
         ShowMessage('Ключ '+BoolToStr(LoadOpenKey(FriendEntry.ID_FRIEND, MailIndex), 'добавлен', 'не добавлен'));
    end;
  end else begin
    // Такой друг есть
    isLoadMessage:= ExistMessage(FriendIndex, GetMailDate(MailIndex));
    if not isLoadMessage then
       ShowMessage('Ключ '+BoolToStr(LoadOpenKey(FriendIndex+1, MailIndex), 'добавлен', 'не добавлен'));
  end;
end;

procedure TCryptChat.GlobalUpdate;
var
  i: integer;
  CMD_UUID: string;
begin
  inherited GlobalUpdate;
  host := 'pop.yandex.ru';
  if not ConnectMail then
    ShowMessage('No connect');
  for i := 1 to GetMailCount do
    // Если сообщение наше
    if SameText(GetMailSubject(i), CHAT_MAIN_SUBJECT) then
    begin
      GetMail(i);
      // то получаем UUID комманды
      CMD_UUID := trim(GetMailCommand(i));
      // выполняем соответствующие действия
      case CMD_UUID of
        CHAT_MAIN_ADD_CONTACT: MainNewContact(i);
        CHAT_INOUT_MESSAGE: MainReadMessage(i);
      end;
    end;
end;

procedure TCryptChat.MainReadMessage(MailIndex: integer);
var
  FriendIndex: integer;
  AFiles: TFiles;
  Path: String;
  i: Integer;
  FromIndex: Integer;
begin
  // Если наше письмо, то выходим
  if SameText(GetUserInfo.Email, GetMailFrom(MailIndex)) then
     exit;
  // Узнаём индекс друга
  FriendIndex:= GetFriendIndex(GetMailFrom(MailIndex));
  if FriendIndex = -1 then
     exit;
  // Если сообщение не загружено, то загружаем
  if not ExistMessage(FriendIndex, GetMailDate(MailIndex)) then
  begin
    fpSystem('rm -fR /tmp/crypt');
    MkDir('/tmp/crypt');
    SetCurrentDir('/tmp/crypt/');
    Path:= '/tmp/crypt/'+GetAttachName(MailIndex, 0);
    if SaveAttachToFile(MailIndex, 0, path) then
    begin
      AddMarkListBox(FriendIndex);
      ExtractBz2(Path, '/tmp/crypt/');
      if ReadMail('/tmp/crypt/', FriendIndex, GetMailDate(MailIndex)) then
      begin // Открыть вкладку
        if not IsOpenPage(FriendIndex) then
        begin // Если вкладка не открыта, то открываем и подгружаем последние 10 ссобщений
           NewPage(FriendIndex);
           if GetCountMessage(FriendIndex)-1 < 10 then
             FromIndex:= 0
           else
             FromIndex:= GetCountMessage(FriendIndex)-10;
        end else // Вкладка открыта, а это значит грузим лишь не достающие сообщения
           FromIndex:= GetCountMessage(FriendIndex)-1; // Кол-во загруженных сообщений
        for i:= FromIndex to GetCountMessage(FriendIndex) - 1 do
        begin
          with GetMessageEntry(i, FriendIndex) do
            if Message <> '' then
              case IsMyMsg of
                TRUE: Send(FriendIndex, Message, Date, nil, TRUE);
                FALSE: Recv(FriendIndex, Message, Date, nil, TRUE);
              end;
        end;
        ScrollDown(FriendIndex);
        DeleteCryptFiles('/tmp/crypt/');
      end;
    end;
    SetCurrentDir(ExtractFilePath(ParamStr(0)));
  end;
end;

procedure TCryptChat.OnTimerUpdate(Sender: TObject);
var
  ThisCount: Integer;
begin
  if isUserLogin then
  begin
     ThisCount:= GetMailCount;
     if (ConnectMail) and (fLastCountMails <> ThisCount) then
     begin
       GlobalUpdate;
       fLastCountMails:= ThisCount;
     end;
  end;
end;

constructor TCryptChat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLastCountMails:= 0;
  UpdateTimer:= TTimer.Create(self);
  UpdateTimer.Interval:= 3000;
  UpdateTimer.OnTimer:= @OnTimerUpdate;
  UpdateTimer.Enabled:= True;
end;

destructor TCryptChat.Destroy;
begin
  UpdateTimer.Enabled:= False;
  UpdateTimer.Free;
  inherited Destroy;
end;

procedure TCryptChat.Send(AFriendID: integer; AText: string; ADate: TDateTime;
  AFiles: TFiles);
var
  TarFile: TStringList;
begin
  { TODO : Проверить отправку сообщения }
  fpSystem('rm -fR /tmp/crypt');
  MkDir('/tmp/crypt');
  SetCurrentDir('/tmp/crypt/');
  if MakeMail('/tmp/crypt/', AText, String(AFiles), AFriendID) then
  begin
    // Закидываем в архив
    MakeBz2('BFKEY.enc MSG.enc Pub.enc', 'mail.tar.bz2');
    TarFile:= TStringList.Create;
    TarFile.Add('mail.tar.bz2');
    HostSmtp:= 'smtp.yandex.ru';
    SendMail(HostSmtp, CHAT_MAIN_SUBJECT, Friend[AFriendID].Email, GetUserInfo.Email,
                   CHAT_INOUT_MESSAGE, '', TarFile);
    TarFile.Free;
    //fpSystem('tar vczf mail.tar.gz *');
    inherited Send(AFriendID, AText, ADate, AFiles);
  end;
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  //fpSystem('rm -fR /tmp/crypt');
end;

procedure TCryptChat.ContactDblClick(Sender: TObject);
var
  i: integer;
  Selected: Integer; // Friend
  FromIndex: Integer;
begin
  if fListBox.ItemIndex = -1 then
     exit;
  for i := 0 to GetCountFriend - 1 do
    with Friend[i] do
    begin
      if fListBox.Items[fListBox.ItemIndex] = (NickName + '< ' + Email + ' >') then
         Selected:= i;
    end;
  inherited ContactDblClick(Sender);
  // Загрузка последних 10 сообщений
  if GetCountMessage(Selected)-1 < 10 then
    FromIndex:= 0
  else
    FromIndex:= GetCountMessage(Selected)-10;
  for i:= FromIndex to GetCountMessage(Selected) - 1 do
  begin
    with GetMessageEntry(i, Selected) do
      if Message <> '' then
        case IsMyMsg of
          TRUE: Send(Selected, Message, Date, nil, TRUE);
          FALSE: Recv(Selected, Message, Date, nil, TRUE);
        end;
  end;
end;

end.
