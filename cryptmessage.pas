unit CryptMessage;

{$mode delphi}

interface

uses
Classes, SysUtils, ExtCtrls, Graphics, StdCtrls, Controls, Dialogs;

const
clHighLightMsg = TColor ($FFC0C0);

type

{ TMessages }

TMessages = class(TCustomControl)
private
  fImg    :TImage;
  fName   :TLabel;
  fText   :TLabel;
  fDate   :TLabel;
  function GetDate   :TDate;
  function GetText   :string;
  function GetTextHeigh (Text   :string)   :integer;
  procedure EnterMouse (Sender   :TObject);
  procedure OnPaintNow (Sender   :TObject);
  function GetUser   :string;
  procedure LeaveMouse (Sender   :TObject);
  procedure SetDate (AValue   :TDate);
  procedure SetText (AValue   :string);
  procedure SetUser (AValue   :string);
public
  constructor Create (TheOwner   :TComponent); override;
  destructor Destroy; override;
  procedure LoadImage (FileName   :string); overload;
  procedure LoadImage (APicture   :TPicture); overload;
  procedure Test;
published
  procedure ReAlign; dynamic;
  property User   :string read GetUser write SetUser;
  property Text   :string read GetText write SetText;
  property Date   :TDate read GetDate write SetDate;
end;

implementation

{ TMessages }

function TMessages.GetTextHeigh (Text   :string)   :integer;
var
  i :integer;
begin
  Result := 0;
  for i := 0 to Length (Text) - 1 do
    if (Text[i] = #13) or (Text[i] = #10) then
      Inc (Result, 1);
  Result := Result * (Canvas.Font.GetTextHeight ('a') + 1);
  //  ShowMessage(Text+'Строк: '+IntToStr(Result)+'; Высота текста: '+IntToStr(Canvas.Font.GetTextHeight('a')));
end;

function TMessages.GetDate   :TDate;
begin
  Result := StrToDate (fDate.Caption);
end;

function TMessages.GetText   :string;
begin
  Result := fText.Caption;
end;

procedure TMessages.EnterMouse (Sender   :TObject);
begin
  if (self is TMessages) then
    (self as TMessages).Color := clHighLightMsg
  else
    (self.Parent as TMessages).Color := clHighLightMsg;
end;

procedure TMessages.OnPaintNow (Sender   :TObject);
var
  DestRect :TRect;
begin
  DestRect.Left   := 4;
  DestRect.Top    := 4;
  DestRect.Bottom := 64;
  DestRect.Right  := 64;
  canvas.StretchDraw (DestRect, fImg.Picture.Graphic);
end;

function TMessages.GetUser   :string;
begin
  Result := fName.Caption;
end;

procedure TMessages.LeaveMouse (Sender   :TObject);
begin
  if (self is TMessages) then
    (self as TMessages).Color := clWhite
  else
    (self.Parent as TMessages).Color := clWhite;
end;

procedure TMessages.SetDate (AValue   :TDate);
begin
  fDate.Caption := DateTimeToStr (AValue);
end;

procedure TMessages.SetText (AValue   :string);
begin
  fText.Caption := AValue;
end;

procedure TMessages.SetUser (AValue   :string);
begin
  fName.Caption := AValue;
end;

constructor TMessages.Create (TheOwner   :TComponent);
begin
  inherited Create (TheOwner);
  // Panel
  Caption   := '';
  //BevelInner:= bvNone;
  //  BevelOuter:=bvNone;
  Color     := clWhite;
  (self as TMessages).Left := 4;
  (self as TMessages).Top := 10;
  (self as TMessages).Height := 100;
  // Avatar
  fImg      := TImage.Create (self as TMessages);
  fImg.Parent := self as TMessages;
  fImg.Left := 4;
  fImg.Top  := 4;
  fImg.Height := 64;
  fImg.Width := 64;
  fImg.Stretch := True;
  fImg.Proportional := True;
  // Name User
  fName     := TLabel.Create (self as TMessages);
  fName.Parent := self as TMessages;
  fName.Left := 84;
  fName.Top := 4;
  fName.Font.Color := clMaroon;
  fName.Font.Style := [fsBold];
  // Text
  fText     := TLabel.Create (self as TMessages);
  fText.Parent := self as TMessages;
  fText.Font.Color := clBlack;
  fText.Left := 84;
  fText.Top := 24;
  fText.WordWrap := True;
  fText.AutoSize := True;
  // Date of Message
  fDate     := TLabel.Create (self as TMessages);
  fDate.Parent := self as TMessages;
  fDate.Left := (self as TMessages).Width - fDate.Width - 4;
  fDate.Anchors := [akRight, akTop];
  fDate.Top := 4;
  fDate.Font.Color := clGray;
  // Mouse events
  (self as TMessages).OnMouseEnter := EnterMouse ();
  (self as TMessages).OnMouseLeave := LeaveMouse ();
  fImg.OnMouseEnter := EnterMouse ();
  fImg.OnMouseLeave := LeaveMouse ();
  fDate.OnMouseEnter := EnterMouse ();
  fDate.OnMouseLeave := LeaveMouse ();
  fText.OnMouseEnter := EnterMouse ();
  fText.OnMouseLeave := LeaveMouse ();
  fName.OnMouseEnter := EnterMouse ();
  fName.OnMouseLeave := LeaveMouse ();
end;

destructor TMessages.Destroy;
begin
  fImg.Free;
  fName.Free;
  fText.Free;
  fDate.Free;
  inherited Destroy;
end;

procedure TMessages.LoadImage (FileName   :string);
begin
  fImg.Picture.LoadFromFile (FileName);
end;

procedure TMessages.LoadImage (APicture   :TPicture);
begin
  fImg.Picture.Assign (APicture);
end;

procedure TMessages.Test;
var
  pic :TPicture;
begin
  //(self as TMessages).OnPaint:= OnPaintNow();
  //fImg.Visible:= false;
  // Test of property
  Text := 'Привет)) Как дела?' + #13 + #10;
  Date := Now;
  User := 'User';
  pic  := TPicture.Create;
  pic.LoadFromFile ('/home/anton/Изображения/ava.jpg');
  LoadImage (pic);
  pic.Free;
end;

procedure TMessages.ReAlign;
begin
  inherited ReAlign;
  (self as TMessages).Height := 60 + GetTextHeigh (string (fText.Caption));
end;

end.

