unit CryptChat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MailCrypt, ExtCtrls, StdCtrls, Graphics, Dialogs, Controls,
  CryptMessage, FileUtil, Forms, Menus, ComCtrls, MainCrypt, ContactBox
  // My controls
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF WINDOWS}  ;

type
  TFiles = pointer;

type
  TPageContact = record
    FriendID: integer;
    FriendName: string;
    Scroll: TScrollBox;
    Panel: TPanel;
    Memo: TMemo;
    MyFace: TImage;
    FriendFace: TImage;
    SendBtn: TButton;
    AtachBtn: TButton;
    FileList: TStringList;
    FileLabel: TLabel;
    Tab:  TTabSheet;
    fMsg: array of TMessages;
  end;

type
  TMsgState = set of (INCOMING, OUTCOMING);

type

  { TLeftWidget }

  TLeftWidget = class(TPageControl)
  public
    procedure DoCloseTabClicked(APage: TCustomPage); override;
  end;

type

  { TCryptChat }

  { TCustomCryptChat }

  TCustomCryptChat = class(TMailCrypt)
    fOpenDialog: TOpenDialog;
    fLeftWidget: TLeftWidget;
    fListBox:    TContactBox;
    procedure ScrollDown(AFriendID: Integer);
  private
    //fMsg:   array of TMessages;
    //* ======================================================================== *//
    fPages: array of TPageContact;
    // Добавить сообщение
    function Add(AFriendID: integer; AText: string; ADate: TDate;
      Avatar: string; AFiles: TFiles; State: TMsgState): boolean;
    // Обработка событий
    procedure MemoKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SendThisMsg(Sender: TObject);
    procedure AttachFiles(Sender: TObject);
    procedure CloseTab(Sender: TObject);
    procedure fUpdateContactList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //* ======================================================================== *//
    //  Если не указан AFriendID, то работа ведётся с активной вкладкой
    //* ======================================================================== *//
    function  IsOpenPage(AFriendId: integer): Boolean;
    function  PageMessageCount(AFriendID: integer): Integer;
    procedure NewPage(AFriendId: integer);
    procedure ContactDblClick(Sender: TObject); virtual;
    procedure Recv(AFriendID: integer; AText: string; ADate: TDateTime; AFiles: TFiles);
      virtual; overload;
    procedure Recv(AFriendID: integer; AText: string; ADate: TDateTime;
      AFiles: TFiles; NoSend: boolean);
      overload;
    procedure Send(AFriendID: integer; AText: string; ADate: TDateTime; AFiles: TFiles);
      virtual; overload;
    procedure Send(AFriendID: integer; AText: string; ADate: TDateTime;
      AFiles: TFiles; NoSend: boolean); overload;
    procedure Recv(AText: string; ADate: TDateTime; AFiles: TFiles); virtual; overload;
    procedure Send(AText: string; ADate: TDateTime; AFiles: TFiles); virtual; overload;
    //* ======================================================================== *//
    // Некоторые вещи придёться вытащить в зону видимости для отладки =(
    //* ======================================================================== *//
    procedure AddMarkListBox(AIndex: Integer);
    procedure ClearMarkListBox(AIndex: Integer);
    procedure GlobalUpdate; virtual;
    function LoginUser(AEMail, APassword: string): boolean; overload; override;
    function LoginUser(AUser: TUserEntry): boolean; overload; override;
  end;

implementation

{ TLeftWidget }

procedure TLeftWidget.DoCloseTabClicked(APage: TCustomPage);
begin
  tag := APage.PageIndex;
  inherited DoCloseTabClicked(APage);
end;

{ TCryptChat }

{$IFDEF WINDOWS}
function WinTemp: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetTempPath(MAX_PATH, PChar(Result)));
end;
{$ENDIF WINDOWS}

procedure TCustomCryptChat.ScrollDown(AFriendID: Integer);
var
  i, CurrentIndex: integer;
begin
  CurrentIndex := -1;
  for i := 0 to High(fPages) do
    if fPages[i].FriendID = AFriendId then
    begin
      CurrentIndex := i;
    end;
  if CurrentIndex = -1 then
    Exit;
  // Выделяем память
  with fPages[CurrentIndex] do
  begin
    Scroll.VertScrollBar.Position := Scroll.VertScrollBar.Range; // ? Ошибка ?
  end;
end;

function TCustomCryptChat.Add(AFriendID: integer; AText: string;
  ADate: TDate; Avatar: string; AFiles: TFiles; State: TMsgState): boolean;
var
  i, CurrentIndex, CountMsg: integer;
begin
  Result := False;
  CurrentIndex := -1;
  for i := 0 to High(fPages) do
    if fPages[i].FriendID = AFriendId then
    begin
      CurrentIndex := i;
    end;
  if CurrentIndex = -1 then
    Exit;
  Result := True;
  // Выделяем память
  with fPages[CurrentIndex] do
  begin
    CountMsg := High(fMsg);
    if CountMsg = -1 then
      SetLength(fMsg, 1)
    else
      SetLength(fMsg, CountMsg + 2);
    CountMsg := High(fMsg);
    // Создаём элемент
    fmsg[CountMsg] := TMessages.Create(Scroll);
    fmsg[CountMsg].Parent := Scroll;
    // ???
    {
    fmsg[CountMsg].OnMouseWheel := Scroll.OnMouseWheel;
    fmsg[CountMsg].OnMouseWheelDown := Scroll.OnMouseWheelDown;
    fmsg[CountMsg].OnMouseWheelUp := Scroll.OnMouseWheelUp;
    }
    // Выставляем позицию сообщения
    if CountMsg > 0 then
      fmsg[CountMsg].Top := fmsg[CountMsg - 1].Top + fmsg[CountMsg - 1].Height + 10
    else
      fmsg[CountMsg].Top := 0;
    fmsg[CountMsg].Align := alTop;
    //fmsg[CountMsg].ReAlign;
    // Содержимое диалога
    fMsg[CountMsg].Date := ADate;
    if State = [INCOMING] then
      fMsg[CountMsg].User := Friend[AFriendID].NickName
    else
      fMsg[CountMsg].User := GetUserInfo.NickName;
    fMsg[CountMsg].Text := AText;
    fMsg[CountMsg].LoadImage(Avatar);
    Scroll.VertScrollBar.Position := Scroll.VertScrollBar.Range; // ? Ошибка ?
  end;
end;

procedure TCustomCryptChat.NewPage(AFriendId: integer);
var
  I: integer;
begin
  // Если диалог уже открыт, просто активируем его страницу
  for i := 0 to High(fPages) do
    if fPages[i].FriendID = AFriendId then
    begin
      fLeftWidget.ActivePage := fPages[i].Tab;
      Exit;
    end;
  // Выделяем память
  if High(fPages) = -1 then
    SetLength(fPages, 1)
  else
    SetLength(fPages, High(fPages) + 2);
  // Дополнительные компоненты
  with fPages[High(fPages)] do
  begin
    FriendID := AFriendId;
    Tab      := fLeftWidget.AddTabSheet;
    fLeftWidget.ActivePage := Tab;
    Tab.Caption := Friend[AFriendId].NickName + '< ' + Friend[AFriendId].Email + ' >';
    Scroll   := TScrollBox.Create(Tab);
    Scroll.Parent := Tab;
    Scroll.VertScrollBar.Tracking := True;
    // Панелька
    Panel    := TPanel.Create(Tab);
    Panel.Parent := Tab;
    Panel.Height := 120;
    Panel.Align := alBottom;
    // Ввод текста
    Memo     := TMemo.Create(Panel);
    Memo.Parent := Panel;
    Memo.OnKeyUp := @MemoKeyUp;
    // Моя фотка
    MyFace   := TImage.Create(Panel);
    MyFace.Parent := Panel;
    {$IFDEF UNIX}
    MyFace.Picture.LoadFromFile('/tmp/usr.png');
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    MyFace.Picture.LoadFromFile(WinTemp + 'usr.png');
    {$ENDIF WINDOWS}
    // Фотка собеседника
    FriendFace := TImage.Create(Panel);
    FriendFace.Parent := Panel;
    {$IFDEF UNIX}
    if not GetFriendImage(AFriendID, '/tmp/friend.png') then
         CopyFile(PChar(ExtractFilePath(ParamStr(0))+'RESOURCE/def_fava.png'),  PChar('/tmp/friend.png'), TRUE);
    FriendFace.Picture.LoadFromFile('/tmp/friend.png');
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    raise Exception.Create('Не написан код для загрузки авы товарища');
    { TODO : Под виндой не загрузиться ава друга }
    {$ENDIF WINDOWS}

    // Расположение
    Scroll.Align := alClient;
    Scroll.AutoScroll := True;
    MyFace.Top   := 4;
    FriendFace.Top := 4;
    MyFace.Left  := 4;
    // Положение фотки собеседника
    FriendFace.Left := Panel.Width - 70;
    FriendFace.Width := 64;
    FriendFace.Proportional := True;
    FriendFace.Anchors := [akRight, akTop];
    FriendFace.Stretch := True;
    // Моего лица
    MyFace.Width := 64;
    MyFace.Stretch := True;
    MyFace.Proportional := True;
    // Положение окна ввода текста
    Memo.Left    := 74;
    Memo.Top     := 4;
    Memo.Height  := Panel.Height - 38;
    Memo.Width   := Panel.Width - 152;
    Memo.Anchors := [akTop, akRight, akBottom, akLeft];
    // Кнопочки
    SendBtn      := TButton.Create(Panel);
    with SendBtn do
    begin
      Parent  := Panel;
      Top     := Panel.Height - 32;
      Left    := 74;
      Width   := 94;
      Height  := 26;
      Caption := 'Отправить';
      OnClick := @SendThisMsg;
    end;
    AtachBtn := TButton.Create(Panel);
    with AtachBtn do
    begin
      Parent  := Panel;
      Top     := Panel.Height - 32;
      Left    := 174;
      Width   := 94;
      Height  := 26;
      Caption := 'Прикрепить';
      OnClick := @AttachFiles;
    end;
    // Список файлов
    FileList  := TStringList.Create;
    FileLabel := TLabel.Create(Panel);
    with FileLabel do
    begin
      Parent  := Panel;
      Caption := 'Прикреплённые файлы (0)';
      Top     := Panel.Height - 28;
      Left    := Memo.Width + 74 - FileLabel.Width;
      Height  := 26;
      font.Color := clNavy;
      Anchors := [akRight, akTop];
    end;
  end;
end;

procedure TCustomCryptChat.MemoKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
const
  VK_RETURN = 13;
begin
  if (ssCtrl in Shift) and (Key = VK_RETURN) then
    SendThisMsg(Sender);
end;

procedure TCustomCryptChat.ContactDblClick(Sender: TObject);
var
  i: integer;
begin
  if fListBox.ItemIndex = -1 then
    exit;
  for i := 0 to GetCountFriend - 1 do
    with Friend[i] do
    begin
      if fListBox.Items[fListBox.ItemIndex] = (NickName + '< ' + Email + ' >') then
        NewPage(i);
    end;
end;

procedure TCustomCryptChat.Recv(AFriendID: integer; AText: string;
  ADate: TDateTime; AFiles: TFiles);
begin
  Recv(AFriendID, AText, ADate, AFiles, TRUE);
end;

procedure TCustomCryptChat.Recv(AFriendID: integer; AText: string;
  ADate: TDateTime; AFiles: TFiles; NoSend: boolean);
var
  Path:   string;
  defAva: string;
begin
  if not NoSend then
    Exit;
  {$IFDEF UNIX}
  Path:= '/tmp/friend.png';
  defAva:= 'RESOURCE/def_fava.png';
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  Path:= WinTemp + 'friend.png';
  defAva:= 'RESOURCE\def_fava.png';
  {$ENDIF WINDOWS}
  if not GetFriendImage(AFriendID, Path) then
    CopyFile(PChar(ExtractFilePath(ParamStr(0)) + defAva), PChar(Path), True);
  if not Add(AFriendID, AText, ADate, Path, AFiles, [INCOMING]) then
    raise Exception.Create('Ошибка получения сообщения');

end;

procedure TCustomCryptChat.Send(AFriendID: integer; AText: string;
  ADate: TDateTime; AFiles: TFiles);
begin
  Send(AFriendID, AText, ADate, AFiles, True);
end;

procedure TCustomCryptChat.Send(AFriendID: integer; AText: string;
  ADate: TDateTime; AFiles: TFiles; NoSend: boolean);
var
  i:    integer;
  Path: string;
begin
  if not NoSend then
    Exit;
  if (AText = '') and (AFiles = nil) then
    exit;
    {$IFDEF UNIX}
    Path:= '/tmp/usr.png';
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    Path:= WinTemp + 'usr.png';
    {$ENDIF WINDOWS}
  if Add(AFriendID, AText, ADate, Path, AFiles, [OUTCOMING]) then
  begin
    for i := 0 to High(fPages) do
      if fPages[i].FriendID = AFriendID then
      begin
        fPages[i].Memo.Clear;
        Break;
      end;
  end
  else
    raise Exception.Create('Ошибка отправки сообщения');
end;

procedure TCustomCryptChat.SendThisMsg(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(fPages) do
    if fPages[I].Tab = fLeftWidget.ActivePage then
      Send(fPages[i].Memo.Text, now, nil);
end;

procedure TCustomCryptChat.Send(AText: string; ADate: TDateTime; AFiles: TFiles);
var
  i: integer;
begin
  for i := 0 to GetCountFriend - 1 do
    with Friend[i] do
    begin
      // i = index of friend != fpages index
      if fLeftWidget.ActivePage.Caption = NickName + '< ' + Email + ' >' then
        Send(i, AText, ADate, AFiles);
    end;
end;

procedure TCustomCryptChat.AddMarkListBox(AIndex: Integer);
begin
  fListBox.NewMessage(AIndex);
end;

procedure TCustomCryptChat.ClearMarkListBox(AIndex: Integer);
begin
  fListBox.ClearMessage(AIndex);
end;

procedure TCustomCryptChat.Recv(AText: string; ADate: TDateTime; AFiles: TFiles);
var
  i: integer;
begin
  for i := 0 to GetCountFriend - 1 do
    with Friend[i] do
    begin
      if fLeftWidget.ActivePage.Caption = NickName + '< ' + Email + ' >' then
        Recv(i, AText, ADate, AFiles);
    end;
end;

procedure TCustomCryptChat.AttachFiles(Sender: TObject);
var
  i: Integer;
begin
  { TODO : Не сделано добавление файлов к сообщениям }
  fOpenDialog:= TOpenDialog.Create(self);
  fOpenDialog.Options:= [ofReadOnly, ofAllowMultiSelect, ofPathMustExist,
                         ofFileMustExist, ofViewDetail, ofAutoPreview];

  if fOpenDialog.Execute then
    for i:= 0 to fOpenDialog.Files.Count - 1 do
      fPages[fLeftWidget.ActivePageIndex].FileList.Add(fOpenDialog.Files[i]);
  fPages[fLeftWidget.ActivePageIndex].FileLabel.Caption:= 'Прикреплённые файлы ('+IntToStr(fPages[fLeftWidget.ActivePageIndex].FileList.Count)+')';
  fOpenDialog.Free;
end;

procedure TCustomCryptChat.CloseTab(Sender: TObject);
var
  ClosedIndex, i: integer;
begin
  ClosedIndex := fLeftWidget.Tag;
  fLeftWidget.ActivePageIndex := ClosedIndex;
  //ClosedIndex := fLeftWidget.TabIndexAtClientPos(ScreenToClient(Mouse.CursorPos));
  if (ClosedIndex = -1) or (ClosedIndex = -2) then
    exit;
  // destroy messages
  for i := 0 to High(fPages[ClosedIndex].fMsg) do
    fPages[ClosedIndex].fMsg[i].Free;
  // Free Page
  with fPages[ClosedIndex] do
  begin
    try
      if Assigned(AtachBtn) then
        FreeAndNil(AtachBtn);
      if Assigned(FileLabel) then
        FreeAndNil(FileLabel);
      if Assigned(FileList) then
        FreeAndNil(FileList);
      if Assigned(FriendFace) then
        FreeAndNil(FriendFace);
      if Assigned(Memo) then
        FreeAndNil(Memo);
      if Assigned(MyFace) then
        FreeAndNil(MyFace);
      if Assigned(Panel) then
        FreeAndNil(Panel);
      if Assigned(Scroll) then
        FreeAndNil(Scroll);
      if Assigned(SendBtn) then
      begin
        SendBtn.OnClick := nil;
        try
          { TODO : Проблемы с уничтожением SendBtn }
          //FreeAndNil(SendBtn); /// ????
          SendBtn := nil; // AV при free ???
        except
        end;
      end;
    finally
      //fLeftWidget.ActivePage.Free;
      if Assigned(Tab) then
        FreeAndNil(Tab);
    end;
  end;
  // correct array
  for i := ClosedIndex to High(fPages) - 1 do
    fPages[i] := fPages[i + 1];
  SetLength(fPages, High(fPages));
end;

procedure TCustomCryptChat.fUpdateContactList;
var
  i: integer;
begin
  for i := 0 to GetCountFriend - 1 do
    with Friend[i] do
    begin
      fListBox.Items.Add(NickName + '< ' + Email + ' >');
      // Нужно установить для каждого элемента теги
    end;
end;

constructor TCustomCryptChat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height   := 400;
  Width    := 400;
  // Всё что слева
  fLeftWidget := TLeftWidget.Create(self);
  fLeftWidget.Options := [nboShowCloseButtons];
  fLeftWidget.OnCloseTabClicked := @CloseTab;
  fLeftWidget.Parent := self;
  fLeftWidget.Height := 100;
  fLeftWidget.Align := alClient;
  fLeftWidget.TabPosition := tpBottom;
  //fLeftWidget.AddTabSheet.Caption := 'Test';
  // Всё что справа
  fListBox := TContactBox.Create(Self);
  with fListBox do
  begin
    Height := 30;
    Left   := 100;
    Top    := 100;
    Width  := 200;
    Align  := alRight;
    Parent := Self;
    OnDblClick := @ContactDblClick;
  end;
  Align := alClient;
end;

destructor TCustomCryptChat.Destroy;
var
  i, j: integer;
begin
  for i := 0 to High(fPages) do
    for j := 0 to High(fPages[i].fMsg) do
      fPages[i].fMsg[j].Free;
  inherited Destroy;
end;

function TCustomCryptChat.IsOpenPage(AFriendId: integer): Boolean;
var
  I: integer;
begin
  Result:= False;
  // Если диалог уже открыт, просто активируем его страницу
  for i := 0 to High(fPages) do
    if fPages[i].FriendID = AFriendId then
    begin
      Result:= True;
      break;
    end;
end;

function TCustomCryptChat.PageMessageCount(AFriendID: integer): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i := 0 to High(fPages) do
    if fPages[i].FriendID = AFriendId then
    begin
      Result:= High(fPages[i].fMsg)+1;
      break;
    end;
end;

procedure TCustomCryptChat.GlobalUpdate;
begin
  fListBox.Clear;
  fUpdateContactList;
end;

function TCustomCryptChat.LoginUser(AEMail, APassword: string): boolean;
var
  Path:  string;
  slash: char;
begin
  Result := inherited LoginUser(AEMail, APassword);
  if Result then
  begin
    {$IFDEF UNIX}
    Path:= '/tmp/usr.png';
    slash:= '/';
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    Path:= WinTemp + 'usr.png';
    slash:= '\';
    {$endif WINDOWS}
    if not GetUserImage(GetUserInfo.ID_USER, Path) then
      CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'RESOURCE' + slash + 'def_ava.png'),
        PChar(Path), True);
  end;
end;

function TCustomCryptChat.LoginUser(AUser: TUserEntry): boolean;
var
  Path:  string;
  slash: char;
begin
  Result := inherited LoginUser(AUser);
  if Result then
  begin
    {$IFDEF UNIX}
    Path:= '/tmp/usr.png';
    slash:= '/';
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    Path:= WinTemp + 'usr.png';
    slash:= '\';
    {$endif WINDOWS}
    if not GetUserImage(GetUserInfo.ID_USER, Path) then
      CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'RESOURCE' + slash + 'def_ava.png'),
        PChar(Path), True);
  end;
end;

end.
