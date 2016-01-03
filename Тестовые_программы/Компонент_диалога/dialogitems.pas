unit DialogItems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, LazUTF8, LCLProc, CommonFunc;

const
  clHighLightMsg = TColor($FFC0C0);

type

  { TDialogItem }

  TDialogItem = class(TCustomControl {CustomPanel})
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
    // Разбить текст на строки, что бы он влазил в определенную ширину
    procedure FormatTextWithMaxWidth(const TextIn: string; StringList: TStringList; MaxWidthPixels: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    // Перестроить размер окна
    procedure ReAlign; dynamic;
    // Заполнение окна
    procedure Establish(AName, AText: string; ATime: TDateTime);
    // Информация о вложениях
    procedure ClearAttachInfo;
    procedure AddAttachInfo(AFileName: string; AOnClick: TNotifyEvent);
    property AttachCount: integer write SetAttachCount;
    // Настройки
    property Picture: TPicture read GetPicture write SetPicture;
    property NameCollocutor: string write SetNameCollocutor;
    property Text: string write SetText;
    property Time: TDateTime write SetTime;
    // Доп.свойства
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;


implementation

{ TDialogItem }

procedure TDialogItem.EnterMouse(Sender: TObject);
// Вход мыши в контрол
begin
  if (self is TDialogItem) then
    (self as TDialogItem).Color := clHighLightMsg
  else
    (self.Parent as TDialogItem).Color := clHighLightMsg;
end;

procedure TDialogItem.LeaveMouse(Sender: TObject);
// Уход мыши из контрола
begin
  if (self is TDialogItem) then
    (self as TDialogItem).Color := clWhite
  else
    (self.Parent as TDialogItem).Color := clWhite;
end;

procedure TDialogItem.EnterMouseForAttachPopup(Sender: TObject);
// Заход мыши на метку вложений
begin
  EnterMouse(Sender);
  fAttachCount.Font.Style := [fsUnderline];
end;

procedure TDialogItem.LeaveMouseForAttachPopup(Sender: TObject);
// Выход мыши из метки вложений
begin
  LeaveMouse(Sender);
  fAttachCount.Font.Style := [];
end;

procedure TDialogItem.OnClickAttach(Sender: TObject);
// Открытия меню вложений
var
  ScreenCoord: TPoint;
begin
  ScreenCoord.X := fAttachCount.Left;
  ScreenCoord.Y := fAttachCount.Top + fAttachCount.Height;
  ScreenCoord := ClientToScreen(ScreenCoord);
  fAttachPopup.PopUp(ScreenCoord.X, ScreenCoord.Y);
end;

function TDialogItem.GetTextHeigh(AText: string): integer;
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

function TDialogItem.GetPicture: TPicture;
begin
  Result := fPicture;
end;

procedure TDialogItem.SetAttachCount(AValue: integer);
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

procedure TDialogItem.SetNameCollocutor(AValue: string);
// Устанавливает имя собеседнику
begin
  fNameCollocutor := AValue;
end;

procedure TDialogItem.SetPicture(AValue: TPicture);
// Установить аватарку
begin
  //  fImage.Picture.Assign(AValue);
  fPicture.Assign(AValue);
end;

procedure TDialogItem.SetText(AValue: string);
// Текст сообщения
begin
  fText := AValue;
end;

procedure TDialogItem.SetTime(AValue: TDateTime);
// Время сообщения
var
  TimeStr: string;
begin
  DateTimeToString(TimeStr, 'DD.MM.YYYY hh:mm:ss', Avalue);
  fTime := TimeStr;
end;

procedure TDialogItem.FormatTextWithMaxWidth(const TextIn: string; StringList: TStringList; MaxWidthPixels: integer);
// Разбить текст на строки, что бы он влазил в определенную ширину
var
  WordList: TStringList;
  WordBuf, CharBuf: string;
  i: integer;
begin
  WordList := TStringList.Create;
  // Создаём список слов
  WordBuf := '';
  for i := 1 to LazUTF8.UTF8Length(TextIn) do
  begin
    CharBuf := LazUTF8.UTF8Copy(TextIn, i, 1);
    if CharBuf = ' ' then
    begin
      WordList.Add(WordBuf);
      WordBuf := '';
    end
    else
      WordBuf := WordBuf + CharBuf;
  end;
  WordList.Add(WordBuf);
  WordBuf := WordBuf + #0 + #0;
  // Подбираем наиболее близкий к максимальной длине текст
  for i := 0 to WordList.Count - 1 do
  begin
    if Canvas.TextWidth(WordList[i]) < MaxWidthPixels then
    ;
  end;
  ShowMessage(WordList.Text);
  WordList.Free;
end;

procedure TDialogItem.Paint;
var
  LeftVal: integer;
  Rect: TRect;
  StringList: TStringList;
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
  LeftVal := (self as TDialogItem).Width - Canvas.GetTextWidth(fTime) - 8;
  Canvas.TextOut(LeftVal, 4, fTime);
  // Текст сообщения
  Canvas.Font.Color := clBlack;
  Canvas.Font.Style := [];
  Rect.Left := 74;
  Rect.Top := 24;
  Rect.Right := LeftVal - 8;
  Rect.Bottom := Height;
  Canvas.TextRect(Rect, 74, 24, fText);
  StringList := TStringList.Create;
  StringList.Free;
  // Изображение аватарки
  Rect.Top := 4;
  Rect.Left := 8;
  Rect.Right := 64;
  Rect.Bottom := 64;
  Canvas.StretchDraw(Rect, fPicture.Graphic);
  // Выводим инфу
  Canvas.TextOut(LeftVal, 40, 'Позиция: ' + IntToStr(BoundsRect.Top));
end;

constructor TDialogItem.Create(AOwner: TComponent);
  // Создание компонентов
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  // Panel
  Caption := '';
  Color := clWhite;
  (self as TDialogItem).Left := 4;
  (self as TDialogItem).Top := 10;
  (self as TDialogItem).Height := 100;
  (self as TDialogItem).BorderStyle := bsNone;
  (self as TDialogItem).DoubleBuffered := True;
  // Avatar
  fPicture := TPicture.Create;
  // Attach count
  fAttachCount := TLabel.Create(self as TDialogItem);
  fAttachCount.Parent := self as TDialogItem;
  fAttachCount.Font.Color := clBlack;
  fAttachCount.Left := (self as TDialogItem).Width - fAttachCount.Width - 8;
  fAttachCount.Top := (self as TDialogItem).Height - (Height - 64 {fImage.Height} - 4{fImage.Top}) - fAttachCount.Height;
  fAttachCount.WordWrap := True;
  fAttachCount.AutoSize := True;
  fAttachCount.Anchors := [akRight, akTop];
  fAttachCount.Font.Color := TColor($8a5f3e);
  AttachCount := 2;
  // Popup menu
  fAttachPopup := TPopupMenu.Create(self as TDialogItem);
  ClearAttachInfo;
  // Mouse events
  (self as TDialogItem).OnMouseEnter := @EnterMouse;
  (self as TDialogItem).OnMouseLeave := @LeaveMouse;
  fAttachCount.OnMouseEnter := @EnterMouseForAttachPopup;
  fAttachCount.OnMouseLeave := @LeaveMouseForAttachPopup;
  fAttachCount.OnClick := @OnClickAttach;
  FormatTextWithMaxWidth('Привет как дела? Чем занимаешься? Давно не видились, хотелось бы пообщаться, если ты не против...', nil, 120);
end;

destructor TDialogItem.Destroy;
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

procedure TDialogItem.ReAlign;
// Изменение размера
begin
  inherited ReAlign;
  // Нужна фича (WordWrap) которая разобьёт строку на строки что бы поместиться в RECT по ширине
  (self as TDialogItem).Height := 74 + GetTextHeigh(string(fText));
end;

procedure TDialogItem.Establish(AName, AText: string; ATime: TDateTime);
// Заполнение данных
begin
  NameCollocutor := AName;
  Text := AText;
  Time := ATime;
end;

procedure TDialogItem.ClearAttachInfo;
// Чистим список вложений
begin
  fAttachPopup.Items.Clear;
  fAttachCount.Visible := False;
end;

procedure TDialogItem.AddAttachInfo(AFileName: string; AOnClick: TNotifyEvent);
// Добавления пункта в меню о вложении
begin
  fAttachPopup.Items.Add(TMenuItem.Create(nil));
  fAttachPopup.Items[fAttachPopup.Items.Count - 1].Caption := AFileName;
  fAttachPopup.Items[fAttachPopup.Items.Count - 1].OnClick := AOnClick;
  AttachCount := fAttachPopup.Items.Count;
  fAttachCount.Visible := True;
end;

end.
