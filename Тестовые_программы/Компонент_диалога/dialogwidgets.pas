unit DialogWidgets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Graphics, StdCtrls, Controls,
  Dialogs, Menus, LazUTF8, fgl, Math;

const
  clHighLightMsg = TColor($FFC0C0);

type

  { TDialogMessage }

  TDialogMessage = class(TCustomPanel)
    // Элемент диалога
  private
    fImage: TImage;
    fNameCollocutor: TLabel;
    fText: TLabel;
    fTime: TLabel;
    fAttachCount: TLabel;
    fAttachPopup: TPopupMenu;
    procedure EnterMouse(Sender: TObject);
    procedure LeaveMouse(Sender: TObject);
    procedure EnterMouseForAttachPopup(Sender: TObject);
    procedure LeaveMouseForAttachPopup(Sender: TObject);
    procedure OnClickAttach(Sender: TObject);
    function GetTextHeigh(AText: string): integer;
  private
    function GetPicture: TPicture;
    procedure SetAttachCount(AValue: integer);
    procedure SetNameCollocutor(AValue: string);
    procedure SetPicture(AValue: TPicture);
    procedure SetText(AValue: string);
    procedure SetTime(AValue: TDateTime);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure ReAlign; dynamic;
    procedure Establish(AName, AText: string; ATime: TDateTime);
    procedure ClearAttachInfo;
    procedure AddAttachInfo(AFileName: string; AOnClick: TNotifyEvent);
    property Picture: TPicture read GetPicture write SetPicture;
    property NameCollocutor: string write SetNameCollocutor;
    property Text: string write SetText;
    property Time: TDateTime write SetTime;
    property AttachCount: integer write SetAttachCount;
  end;

  TAttachInfo = class(TObject)
    // Информация о вложении
    Name: string;
    OnClick: TNotifyEvent;
  end;

  TAttachList = specialize fgl.TFPGObjectList<TAttachInfo>;

  { TMessageData }

  TMessageData = class(TObject)
    // Данные сообщения
    Text: string;
    Time: TDateTime;
    AttachList: TAttachList;
    Picture: TPicture;
    Message: TDialogMessage;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TMessageDataList = specialize fgl.TFPGObjectList<TMessageData>;

  TDialog = class(TCustomPanel)
    // Диалог
  private
    fFriendName: string;
    fUserName: string;
    fUserPicture: TPicture;
    fFriendPicture: TPicture;
    fItems: TMessageDataList;
    fPanel: TCustomPanel;
    fScrollBar: TScrollBar;
    function GetCount: integer;
    function GetMessageData(Index: integer): TMessageData;
    procedure SetMessageData(Index: integer; AValue: TMessageData);
    procedure OnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: integer);
    procedure OnChangeScroll(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Add(AText: string; ATime: TDateTime; AIsFriend: boolean);
    property UserPicture: TPicture read fUserPicture write fUserPicture;
    property UserName: string read fUserName write fUserName;
    property FriendPicture: TPicture read fFriendPicture write fFriendPicture;
    property FriendName: string read fFriendName write fFriendName;
    property Items[Index: integer]: TMessageData
      read GetMessageData write SetMessageData;
    property Count: integer read GetCount;
  end;

implementation

procedure Nop;
begin
  // Нет действий
end;

{ TMessageData }

constructor TMessageData.Create;
begin
  AttachList := TAttachList.Create(True);
  Picture := TPicture.Create;
  Message := TDialogMessage.Create(nil);
  inherited Create;
end;

destructor TMessageData.Destroy;
begin
  if Assigned(AttachList) then
    AttachList.Free;
  if Assigned(Picture) then
    Picture.Free;
  if Assigned(Message) then
    Message.Free;
  inherited Destroy;
end;

{ TDialog }

function TDialog.GetMessageData(Index: integer): TMessageData;
begin
  Result := fItems[Index];
end;

function TDialog.GetCount: integer;
begin
  Result := fItems.Count;
end;

procedure TDialog.SetMessageData(Index: integer; AValue: TMessageData);
begin
  fItems[Index] := AValue;
end;

procedure TDialog.OnScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
var
  delta: integer;
begin
  delta := -1 * (ScrollPos + Items[0].Message.Top);
  case ScrollCode of
    scLineUp: fPanel.ScrollBy(0, delta);   // = SB_LINEUP
    scLineDown: fPanel.ScrollBy(0, delta); // = SB_LINEDOWN
  //  scPageUp: fPanel.ScrollBy(0, delta * 10);   // = SB_PAGEUP
    scPageDown: fPanel.ScrollBy(0, delta * 10); // = SB_PAGEDOWN
    scPosition: fPanel.ScrollBy(0, delta); // = SB_THUMBPOSITION
    scTrack: fPanel.ScrollBy(0, delta);    // = SB_THUMBTRACK
  //  scTop: fPanel.ScrollBy(0, delta);      // = SB_TOP
  //  scBottom: fPanel.ScrollBy(0, delta);   // = SB_BOTTOM
//    scEndScroll: fPanel.ScrollBy(0, delta); // = SB_ENDSCROLL
  end;
end;

procedure TDialog.OnChangeScroll(Sender: TObject);
begin
  //  fPanel.ScrollBy(0, 1)
end;

constructor TDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.BevelInner := bvNone;
  Self.BevelOuter := bvRaised;
  fItems := TMessageDataList.Create(True);
  fUserPicture := TPicture.Create;
  fFriendPicture := TPicture.Create;
  // Компонент
  fScrollBar := TScrollBar.Create(self);
  fScrollBar.Kind := sbVertical;
  fScrollBar.Align := alRight;
  fScrollBar.Parent := self;
  fScrollBar.OnScroll := @OnScroll;
  fScrollBar.OnChange := @OnChangeScroll;
  fPanel := TCustomPanel.Create(Self);
  fPanel.Parent := self;
  fPanel.Align := alClient;
end;

destructor TDialog.Destroy;
begin
  if Assigned(fPanel) then
    fPanel.Free;
  if Assigned(fScrollBar) then
    fScrollBar.Free;
  if Assigned(fItems) then
    fItems.Free;
  if Assigned(fUserPicture) then
    fUserPicture.Free;
  if Assigned(fFriendPicture) then
    fFriendPicture.Free;
  inherited Destroy;
end;

procedure TDialog.Add(AText: string; ATime: TDateTime; AIsFriend: boolean);
var
  Data: TMessageData;
begin
  // Запоминаем параметры
  Data := TMessageData.Create;
  Data.Text := AText;
  Data.Time := ATime;
  if AIsFriend then
    Data.Picture.Assign(FriendPicture)
  else
    Data.Picture.Assign(UserPicture);
  // Добавляем визуальный элемент
  Data.Message.Left := 0;
  if Count = 0 then
    Data.Message.Top := 0
  else
    Data.Message.Top := Items[Count - 1].Message.Top + Items[Count - 1].Message.Height;
  if Data.Message.Top > Height then
    fScrollBar.Visible := True;
  Data.Message.Width := fPanel.Width;
  Data.Message.Parent := fPanel;
  Data.Message.Anchors := [akLeft, akRight, akTop];
  // заполняем
  if AIsFriend then
  begin
    Data.Message.Establish(FriendName, Data.Text, Data.Time);
    Data.Message.Picture := FriendPicture;
  end
  else
  begin
    Data.Message.Establish(UserName, Data.Text, Data.Time);
    Data.Message.Picture := UserPicture;
  end;
  //  Message.AddAttachInfo('Инструкция.pdf', nil);
  fItems.Add(Data);

  fScrollBar.Max := Items[Count - 1].Message.Top;
end;

{ TDialogMessage }

procedure TDialogMessage.EnterMouse(Sender: TObject);
// Вход мыши в контрол
begin
  if (self is TDialogMessage) then
    (self as TDialogMessage).Color := clHighLightMsg
  else
    (self.Parent as TDialogMessage).Color := clHighLightMsg;
end;

procedure TDialogMessage.LeaveMouse(Sender: TObject);
// Уход мыши из контрола
begin
  if (self is TDialogMessage) then
    (self as TDialogMessage).Color := clWhite
  else
    (self.Parent as TDialogMessage).Color := clWhite;
end;

procedure TDialogMessage.EnterMouseForAttachPopup(Sender: TObject);
// Заход мыши на метку вложений
begin
  EnterMouse(Sender);
  fAttachCount.Font.Style := [fsUnderline];
end;

procedure TDialogMessage.LeaveMouseForAttachPopup(Sender: TObject);
// Выход мыши из метки вложений
begin
  LeaveMouse(Sender);
  fAttachCount.Font.Style := [];
end;

procedure TDialogMessage.OnClickAttach(Sender: TObject);
// Открытия меню вложений
var
  ScreenCoord: TPoint;
begin
  ScreenCoord.X := fAttachCount.Left;
  ScreenCoord.Y := fAttachCount.Top + fAttachCount.Height;
  ScreenCoord := ClientToScreen(ScreenCoord);
  fAttachPopup.PopUp(ScreenCoord.X, ScreenCoord.Y);
end;

function TDialogMessage.GetTextHeigh(AText: string): integer;
  // Получить высоту текста
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(AText) - 1 do
    if (AText[i] = #13) or (AText[i] = #10) then
      Inc(Result, 1);
  Result := Result * (Canvas.Font.GetTextHeight('a') + 1);
end;

function TDialogMessage.GetPicture: TPicture;
begin
  Result := fImage.Picture;
end;

procedure TDialogMessage.SetAttachCount(AValue: integer);
// устанавливает количество файлов в сообщении
var
  StrDesc: string;
  LastCh: integer;
begin
  StrDesc := '';
  LastCh := StrToInt(IntToStr(AValue)[Length(IntToStr(AValue))]);
  if LastCh in [2, 3, 4] then
    StrDesc := ' вложения'
  else if LastCh in [1] then
    StrDesc := ' вложение'
  else
    StrDesc := ' вложений';
  fAttachCount.Caption := UTF8ToSys(' ') + IntToStr(AValue) + StrDesc;
end;

procedure TDialogMessage.SetNameCollocutor(AValue: string);
// Устанавливает имя собеседнику
begin
  fNameCollocutor.Caption := AValue;
end;

procedure TDialogMessage.SetPicture(AValue: TPicture);
// Установить аватарку
begin
  fImage.Picture.Assign(AValue);
end;

procedure TDialogMessage.SetText(AValue: string);
// Текст сообщения
begin
  fText.Caption := AValue;
end;

procedure TDialogMessage.SetTime(AValue: TDateTime);
// Время сообщения
var
  TimeStr: string;
begin
  DateTimeToString(TimeStr, 'DD.MM.YYYY hh:mm:ss', Avalue);
  fTime.Caption := TimeStr;
end;

constructor TDialogMessage.Create(AOwner: TComponent);
  // Создание компонентов
begin
  inherited Create(AOwner);
  // Panel
  Caption := '';
  Color := clWhite;
  (self as TDialogMessage).Left := 4;
  (self as TDialogMessage).Top := 10;
  (self as TDialogMessage).Height := 100;
  (self as TDialogMessage).BorderStyle := bsNone;
  (self as TDialogMessage).BevelInner := bvNone;
  (self as TDialogMessage).BevelOuter := bvNone;
  // Avatar
  fImage := TImage.Create(self as TDialogMessage);
  fImage.Parent := self as TDialogMessage;
  fImage.Left := 8;
  fImage.Top := 4;
  fImage.Height := 64;
  fImage.Width := 64;
  fImage.Stretch := True;
  fImage.Proportional := True;
  fImage.Transparent := True;
  // Name User
  fNameCollocutor := TLabel.Create(self as TDialogMessage);
  fNameCollocutor.Parent := self as TDialogMessage;
  fNameCollocutor.Left := 74;
  fNameCollocutor.Top := 4;
  fNameCollocutor.Font.Color := TColor($8a5f3e);
  fNameCollocutor.Font.Style := [fsBold];
  // Text
  fText := TLabel.Create(self as TDialogMessage);
  fText.Parent := self as TDialogMessage;
  fText.Font.Color := clBlack;
  fText.Left := 74;
  fText.Top := 24;
  fText.WordWrap := True;
  fText.AutoSize := True;
  // Attach count
  fAttachCount := TLabel.Create(self as TDialogMessage);
  fAttachCount.Parent := self as TDialogMessage;
  fAttachCount.Font.Color := clBlack;
  fAttachCount.Left := (self as TDialogMessage).Width - fAttachCount.Width - 8;
  fAttachCount.Top := (self as TDialogMessage).Height -
    (Height - fImage.Height - fImage.Top) - fAttachCount.Height;
  fAttachCount.WordWrap := True;
  fAttachCount.AutoSize := True;
  fAttachCount.Anchors := [akRight, akTop];
  fAttachCount.Font.Color := TColor($8a5f3e);
  AttachCount := 2;
  // Date of Message
  fTime := TLabel.Create(self as TDialogMessage);
  fTime.Parent := self as TDialogMessage;
  fTime.Left := (self as TDialogMessage).Width - fTime.Width - 8;
  fTime.Anchors := [akRight, akTop];
  fTime.Top := 4;
  fTime.Font.Color := clGray;
  // Popup menu
  fAttachPopup := TPopupMenu.Create(self as TDialogMessage);
  ClearAttachInfo;
  // Mouse events
  (self as TDialogMessage).OnMouseEnter := @EnterMouse;
  (self as TDialogMessage).OnMouseLeave := @LeaveMouse;
  fImage.OnMouseEnter := @EnterMouse;
  fImage.OnMouseLeave := @LeaveMouse;
  fTime.OnMouseEnter := @EnterMouse;
  fTime.OnMouseLeave := @LeaveMouse;
  fText.OnMouseEnter := @EnterMouse;
  fText.OnMouseLeave := @LeaveMouse;
  fNameCollocutor.OnMouseEnter := @EnterMouse;
  fNameCollocutor.OnMouseLeave := @LeaveMouse;
  fAttachCount.OnMouseEnter := @EnterMouseForAttachPopup;
  fAttachCount.OnMouseLeave := @LeaveMouseForAttachPopup;
  fAttachCount.OnClick := @OnClickAttach;
end;

destructor TDialogMessage.Destroy;
  // Уничтожение компонента
begin
  try
    if Assigned(fImage) then
      fImage.Free;
    if Assigned(fNameCollocutor) then
      fNameCollocutor.Free;
    if Assigned(fText) then
      fText.Free;
    if Assigned(fTime) then
      fTime.Free;
    if Assigned(fAttachCount) then
      fAttachCount.Free;
    if Assigned(fAttachPopup) then
      fAttachPopup.Free;
  except
    // Пропускаем ошибки если есть ибо не критично
    Nop;
  end;
  inherited Destroy;
end;

procedure TDialogMessage.ReAlign;
// Изменение размера
begin
  inherited ReAlign;
  (self as TDialogMessage).Height := 74 + GetTextHeigh(string(fText.Caption));
end;

procedure TDialogMessage.Establish(AName, AText: string; ATime: TDateTime);
// Заполнение данных
begin
  NameCollocutor := AName;
  Text := AText;
  Time := ATime;
end;

procedure TDialogMessage.ClearAttachInfo;
// Чистим список вложений
begin
  fAttachPopup.Items.Clear;
  fAttachCount.Visible := False;
end;

procedure TDialogMessage.AddAttachInfo(AFileName: string; AOnClick: TNotifyEvent);
// Добавления пункта в меню о вложении
begin
  fAttachPopup.Items.Add(TMenuItem.Create(nil));
  fAttachPopup.Items[fAttachPopup.Items.Count - 1].Caption := AFileName;
  fAttachPopup.Items[fAttachPopup.Items.Count - 1].OnClick := AOnClick;
  AttachCount := fAttachPopup.Items.Count;
  fAttachCount.Visible := True;
end;

end.
