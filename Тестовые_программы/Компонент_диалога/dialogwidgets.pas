unit DialogWidgets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Graphics, StdCtrls, Controls,
  Dialogs, Menus, LazUTF8, fgl;

const
  clHighLightMsg = TColor($FFC0C0);

type

  { TDialogMessage }

  TDialogMessage = class(TCustomControl {CustomPanel})
    // Элемент диалога
  private
    fPicture: TPicture;
    fNameCollocutor: string; // Имя собеседника
    fText: string;
    fTime: string;
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
  protected
    procedure Paint; override;
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
    property OnMouseWheel;
  end;

  TAttachInfo = class(TObject)
    // Информация о вложении
    Name: string;
    OnClick: TNotifyEvent;
  end;

  TAttachList = specialize TFPGObjectList<TAttachInfo>;

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

  TMessageDataList = specialize TFPGObjectList<TMessageData>;

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
    procedure OnEventMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
    procedure OnEventMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Add(AText: string; ATime: TDateTime; AIsFriend: boolean);
    property UserPicture: TPicture read fUserPicture write fUserPicture;
    property UserName: string read fUserName write fUserName;
    property FriendPicture: TPicture read fFriendPicture write fFriendPicture;
    property FriendName: string read fFriendName write fFriendName;
    property Items[Index: integer]: TMessageData read GetMessageData write SetMessageData;
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

procedure TDialog.OnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: integer);
var
  delta: integer;
begin
  delta := -1 * (ScrollPos + Items[0].Message.Top);
  case ScrollCode of
    scLineUp: fPanel.ScrollBy(0, delta);   // = SB_LINEUP
    scLineDown: fPanel.ScrollBy(0, delta); // = SB_LINEDOWN
    scPageUp: fPanel.ScrollBy(0, delta * -10);   // = SB_PAGEUP
    scPageDown: fPanel.ScrollBy(0, delta * 10); // = SB_PAGEDOWN
    scPosition: fPanel.ScrollBy(0, delta); // = SB_THUMBPOSITION
    scTrack: fPanel.ScrollBy(0, delta);    // = SB_THUMBTRACK
  end;
end;

procedure TDialog.OnEventMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
// Прокрутка колёсиком мыши вверх
var
  delta: integer;
begin
  fScrollBar.Position := fScrollBar.Position - 10;
  delta := -1 * (fScrollBar.Position + Items[0].Message.Top);
  fPanel.BeginUpdateBounds;
  fPanel.ScrollBy(0, delta);
  fPanel.EndUpdateBounds;
  Application.ProcessMessages;
end;

procedure TDialog.OnEventMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
// Прокрутка колёсиком мыши вниз
var
  delta: integer;
begin
  fScrollBar.Position := fScrollBar.Position + 10;
  delta := -1 * (fScrollBar.Position + Items[0].Message.Top);
  fPanel.BeginUpdateBounds;
  fPanel.ScrollBy(0, delta);
  fPanel.EndUpdateBounds;
  Application.ProcessMessages;
end;

constructor TDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnMouseWheelDown := @OnEventMouseWheelDown;
  OnMouseWheelUp := @OnEventMouseWheelUp;
  DoubleBuffered := True;
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
  Data.Message.OnMouseWheelUp := @OnEventMouseWheelUp;
  Data.Message.OnMouseWheelDown := @OnEventMouseWheelDown;

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
  Result := fPicture;
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
  fNameCollocutor := AValue;
end;

procedure TDialogMessage.SetPicture(AValue: TPicture);
// Установить аватарку
begin
  //  fImage.Picture.Assign(AValue);
  fPicture.Assign(AValue);
end;

procedure TDialogMessage.SetText(AValue: string);
// Текст сообщения
begin
  fText := AValue;
end;

procedure TDialogMessage.SetTime(AValue: TDateTime);
// Время сообщения
var
  TimeStr: string;
begin
  DateTimeToString(TimeStr, 'DD.MM.YYYY hh:mm:ss', Avalue);
  fTime := TimeStr;
end;

procedure TDialogMessage.Paint;
var
  LeftVal: integer;
  Rect: TRect;
begin
  // Для более быстрого рисования имеет смысл отключать OnPaint когда компонент не видим
  inherited Paint;
  // Имя
  Canvas.Font.Color := TColor($8a5f3e);
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(74, 4, fNameCollocutor);
  // Время сообщения
  Canvas.Font.Color := clGray;
  Canvas.Font.Style := [];
  LeftVal := (self as TDialogMessage).Width - Canvas.GetTextWidth(fTime) - 8;
  Canvas.TextOut(LeftVal, 4, fTime);
  // Текст сообщения
  Canvas.Font.Color := clBlack;
  Canvas.Font.Style := [];
  Rect.Left := 74;
  Rect.Top := 24;
  Rect.Right := LeftVal - 8;
  Rect.Bottom := Height;
  Canvas.TextRect(Rect, 74, 24, fText);
  // Изображение аватарки
  Rect.Top := 4;
  Rect.Left := 8;
  Rect.Right := 64;
  Rect.Bottom := 64;
  Canvas.StretchDraw(Rect, fPicture.Graphic);
  // Выводим инфу
  Canvas.TextOut(LeftVal, 40, 'Позиция: ' + IntToStr(BoundsRect.Top));
end;

constructor TDialogMessage.Create(AOwner: TComponent);
  // Создание компонентов
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  // Panel
  Caption := '';
  Color := clWhite;
  (self as TDialogMessage).Left := 4;
  (self as TDialogMessage).Top := 10;
  (self as TDialogMessage).Height := 100;
  (self as TDialogMessage).BorderStyle := bsNone;
  (self as TDialogMessage).DoubleBuffered := True;
  // Avatar
  fPicture := TPicture.Create;
  // Attach count
  fAttachCount := TLabel.Create(self as TDialogMessage);
  fAttachCount.Parent := self as TDialogMessage;
  fAttachCount.Font.Color := clBlack;
  fAttachCount.Left := (self as TDialogMessage).Width - fAttachCount.Width - 8;
  fAttachCount.Top := (self as TDialogMessage).Height - (Height - 64 {fImage.Height} - 4{fImage.Top}) - fAttachCount.Height;
  fAttachCount.WordWrap := True;
  fAttachCount.AutoSize := True;
  fAttachCount.Anchors := [akRight, akTop];
  fAttachCount.Font.Color := TColor($8a5f3e);
  AttachCount := 2;
  // Popup menu
  fAttachPopup := TPopupMenu.Create(self as TDialogMessage);
  ClearAttachInfo;
  // Mouse events
  (self as TDialogMessage).OnMouseEnter := @EnterMouse;
  (self as TDialogMessage).OnMouseLeave := @LeaveMouse;
  fAttachCount.OnMouseEnter := @EnterMouseForAttachPopup;
  fAttachCount.OnMouseLeave := @LeaveMouseForAttachPopup;
  fAttachCount.OnClick := @OnClickAttach;
end;

destructor TDialogMessage.Destroy;
  // Уничтожение компонента
begin
  try
    if Assigned(fAttachCount) then
      fAttachCount.Free;
    if Assigned(fAttachPopup) then
      fAttachPopup.Free;
    if Assigned(fPicture) then
      fPicture.Free;
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
  // Нужна фича (WordWrap) которая разобьёт строку на строки что бы поместиться в RECT по ширине
  (self as TDialogMessage).Height := 74 + GetTextHeigh(string(fText));
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
