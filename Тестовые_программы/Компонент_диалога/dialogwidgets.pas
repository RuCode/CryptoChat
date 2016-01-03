unit DialogWidgets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Graphics, StdCtrls, Controls,
  Dialogs, Menus, LazUTF8, fgl, DialogItems, Math, CommonFunc;

type

  { TDialogMessage }


  TAttachInfo = class(TObject)
    // Информация о вложении
    Id: integer;
    Name: string;
    OnClick: TNotifyEvent;
  end;

  TAttachList = specialize TFPGObjectList<TAttachInfo>;

  { TMessageData }

  TMessageData = class(TObject)
    // Данные сообщения
    TitleName: string;
    Text: string;
    Time: TDateTime;
    AttachList: TAttachList;
    Picture: TPicture;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TMessageDataList = specialize TFPGObjectList<TMessageData>;
  TDialogItemsList = specialize TFPGObjectList<TDialogItem>;

  TDialog = class(TCustomPanel)
    // Диалог
  private
    // Реальные окна сообщений
    function GetRealMessageItem(Index: integer): TDialogItem;
    procedure SetRealMessageItem(Index: integer; AValue: TDialogItem);
    // Виртуальные сообщения
    function GetMessage(Index: integer): TMessageData;
    procedure SetMessage(Index: integer; AValue: TMessageData);
    function GetMessageCount: integer;
  private
    fFriendName: string;
    fUserName: string;
    fUserPicture: TPicture;
    fFriendPicture: TPicture;
    fPanel: TCustomPanel;
    fScrollBar: TScrollBar;
    fMessages: TMessageDataList;
    fRealMessageItems: TDialogItemsList;
    // Прокрутка окна компонента
    function GetRealMessageCount: integer;
    procedure OnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: integer);
    procedure OnEventMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
    procedure OnEventMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
    // Создание реального элемента сообщения
    function AddRealMessage: integer;
    // Присвоить данные из виртуального сообщение реальному
    procedure RealMessageLoadDataFromVirtualMessage(IndexOfRealMessage, IndexOfVirtualMessage: integer);
    // Вернуть верхнею свободную границу в окне
    function GetYCoordForNewMessage: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure Resize; override;
  public
    // Настройка компонента
    property UserPicture: TPicture read fUserPicture write fUserPicture;
    property UserName: string read fUserName write fUserName;
    property FriendPicture: TPicture read fFriendPicture write fFriendPicture;
    property FriendName: string read fFriendName write fFriendName;
    // Добавление виртуального сообщения
    procedure Add(AText: string; ATime: TDateTime; AIsFriend: boolean); overload;
    procedure Add(AText: string; ATime: TDateTime; AIsFriend: boolean; AAttachList: TAttachList); overload;
    // Количество сообщений
    property MessageCount: integer read GetMessageCount;
    property RealMessageCount: integer read GetRealMessageCount;
    // Сообщения виртуальные
    property Message[Index: integer]: TMessageData read GetMessage write SetMessage;
    // Количество окон для вывода сообщений
    property RealMessageItem[Index: integer]: TDialogItem read GetRealMessageItem write SetRealMessageItem;
  end;

implementation

{ TMessageData }

constructor TMessageData.Create;
begin
  AttachList := TAttachList.Create(True);
  Picture := TPicture.Create;
  inherited Create;
end;

destructor TMessageData.Destroy;
begin
  if Assigned(AttachList) then
    AttachList.Free;
  if Assigned(Picture) then
    Picture.Free;
  inherited Destroy;
end;

{ TDialog }

constructor TDialog.Create(AOwner: TComponent);
  // Создание компонента
begin
  inherited Create(AOwner);
  OnMouseWheelDown := @OnEventMouseWheelDown;
  OnMouseWheelUp := @OnEventMouseWheelUp;
  DoubleBuffered := True;
  Self.BevelInner := bvNone;
  Self.BevelOuter := bvRaised;
  fMessages := TMessageDataList.Create(True);
  fRealMessageItems := TDialogItemsList.Create(False);  // <- Графические компоненты удаляются при удалении предков (автоматически)
  fUserPicture := TPicture.Create;
  fFriendPicture := TPicture.Create;
  // Компонент
  fScrollBar := TScrollBar.Create(self);
  fScrollBar.Kind := sbVertical;
  fScrollBar.Align := alRight;
  fScrollBar.Parent := self;
  fScrollBar.OnScroll := @OnScroll;
  fScrollBar.Max := 1;
  fScrollBar.Min := 1;
  fPanel := TCustomPanel.Create(Self);
  fPanel.Parent := self;
  fPanel.Align := alClient;
end;

destructor TDialog.Destroy;
  // Уничтожение
begin
  if Assigned(fPanel) then
    fPanel.Free;
  if Assigned(fScrollBar) then
    fScrollBar.Free;
  if Assigned(fMessages) then
    fMessages.Free;
  if Assigned(fRealMessageItems) then
    fRealMessageItems.Free;
  if Assigned(fUserPicture) then
    fUserPicture.Free;
  if Assigned(fFriendPicture) then
    fFriendPicture.Free;
  inherited Destroy;
end;

function TDialog.GetMessage(Index: integer): TMessageData;
begin
  Result := fMessages[Index];
end;

function TDialog.GetRealMessageItem(Index: integer): TDialogItem;
begin
  Result := fRealMessageItems[Index];
end;

procedure TDialog.SetRealMessageItem(Index: integer; AValue: TDialogItem);
begin
  fRealMessageItems[Index] := AValue;
end;

function TDialog.GetMessageCount: integer;
begin
  Result := fMessages.Count;
end;

procedure TDialog.SetMessage(Index: integer; AValue: TMessageData);
begin
  fMessages[Index] := AValue;
end;

procedure TDialog.OnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: integer);
var
  delta: integer;
begin
  if MessageCount = 0 then
    exit;
  delta := -1 * (ScrollPos + RealMessageItem[0].Top);
  case ScrollCode of
    scLineUp: fPanel.ScrollBy(0, delta);   // = SB_LINEUP
    scLineDown: fPanel.ScrollBy(0, delta); // = SB_LINEDOWN
    scPageUp: fPanel.ScrollBy(0, delta * -10);   // = SB_PAGEUP
    scPageDown: fPanel.ScrollBy(0, delta * 10); // = SB_PAGEDOWN
    scPosition: fPanel.ScrollBy(0, delta); // = SB_THUMBPOSITION
    scTrack: fPanel.ScrollBy(0, delta);    // = SB_THUMBTRACK
    else
      nop;
  end;
end;

function TDialog.GetRealMessageCount: integer;
begin
  Result := fRealMessageItems.Count;
end;

procedure TDialog.OnEventMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
// Прокрутка колёсиком мыши вверх
var
  delta: integer;
begin
  if MessageCount = 0 then
    exit;
  fScrollBar.Position := fScrollBar.Position - 10;
  delta := -1 * (fScrollBar.Position + RealMessageItem[0].Top);
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
  if MessageCount = 0 then
    exit;
  fScrollBar.Position := fScrollBar.Position + 10;
  delta := -1 * (fScrollBar.Position + RealMessageItem[0].Top);
  fPanel.BeginUpdateBounds;
  fPanel.ScrollBy(0, delta);
  fPanel.EndUpdateBounds;
  Application.ProcessMessages;
end;

procedure TDialog.Resize;
begin
  inherited Resize;
end;

function TDialog.AddRealMessage: integer;
  // Создание реального элемента сообщения
var
  Item: TDialogItem;
begin
  Item := TDialogItem.Create(fPanel);
  Item.Left := 0;
  Item.Top := ifthen(RealMessageCount = 0, 0, GetYCoordForNewMessage);
  fScrollBar.Visible := Item.Top > Height;
  Item.Width := fPanel.Width;
  Item.Parent := fPanel;
  Item.Anchors := [akLeft, akRight, akTop];
  Item.OnMouseWheelUp := @OnEventMouseWheelUp;
  Item.OnMouseWheelDown := @OnEventMouseWheelDown;
  Item.ReAlign;
  fRealMessageItems.Add(Item);
  Result := fRealMessageItems.Count - 1;
end;

procedure TDialog.RealMessageLoadDataFromVirtualMessage(IndexOfRealMessage, IndexOfVirtualMessage: integer);
// Присвоить данные из виртуального сообщение реальному
begin
  with fMessages[IndexOfVirtualMessage] do
    fRealMessageItems[IndexOfRealMessage].Establish(TitleName, Text, Time);
  fRealMessageItems[IndexOfRealMessage].Picture.Assign(fMessages[IndexOfVirtualMessage].Picture);
end;

function TDialog.GetYCoordForNewMessage: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i:= 0 to RealMessageCount - 1 do
    Result += RealMessageItem[i].Height;
end;

procedure TDialog.Add(AText: string; ATime: TDateTime; AIsFriend: boolean);
begin
  Add(AText, ATime, AIsFriend, nil);
end;

procedure TDialog.Add(AText: string; ATime: TDateTime; AIsFriend: boolean; AAttachList: TAttachList);
// Добавление виртуального элемента
var
  Data: TMessageData;
begin
  // Заполняем данные
  Data := TMessageData.Create;
  with Data do
  begin
    TitleName := BoolToStr(AIsFriend, FriendName, UserName);
    Text := AText;
    Time := ATime;
    if Assigned(AAttachList) then
      AttachList.Assign(AAttachList);
    if AIsFriend then
      Data.Picture.Assign(FriendPicture)
    else
      Data.Picture.Assign(UserPicture);
  end;
  fMessages.Add(Data);
  // Проверяем можем ли мы добавить видимый элемент
  RealMessageLoadDataFromVirtualMessage(AddRealMessage, fMessages.Count - 1);
end;


end.
