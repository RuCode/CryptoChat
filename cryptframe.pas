unit CryptFrame;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
Dialogs, Menus, ExtCtrls, StdCtrls,
// My controls
CryptMessage;

type
TFiles = pointer;

type

{ TCryptFrame }

TCryptFrame = class(TWinControl)
private
  fScroll   :TScrollBox;
  fPanel    :TPanel;
  fMemo     :TMemo;
  fMyFace   :TImage;
  fFriendFace :TImage;
  // Мои данные
  fUserName :string;
  // Данные собеседника
  fFriendName :string;
  // Список сообщений
  fMsg      :array of TMessages;
  function GetFriendName :string;
  function GetUserName :string;
  procedure SetUserName (UserName :string);
  procedure SetFriendName (UserName :string);
  procedure MemoKeyUp (Sender :TObject; var Key :word; Shift :TShiftState);
public
  constructor Create (AOwner :TComponent); override;
  // Добавить сообщение
  function Add (UserName, szText :string; Date :TDate; Avatar :TPicture;
    Files :TFiles) :boolean;
  // Количество сообщений
  function Count :integer;
  // Аватарки
  procedure SetUserPic (FileName :string);
  procedure SetFriendPic (FileName :string);
  procedure Debug;
published
  property UserName :string read GetUserName write SetUserName;
  property FriendName :string read GetFriendName write SetFriendName;
end;


implementation

{ TCryptFrame }

function TCryptFrame.GetFriendName :string;
begin
  Result := fFriendName;
end;

function TCryptFrame.GetUserName :string;
begin
  Result := fFriendName;
end;

constructor TCryptFrame.Create (AOwner :TComponent);
begin
  inherited Create (AOwner);
  // Дополнительные компоненты
  fScroll    := TScrollBox.Create (self);
  fScroll.Parent := self;
  fPanel     := TPanel.Create (self);
  fPanel.Parent := self;
  fPanel.Height := 100;
  fPanel.Align := alBottom;
  fMemo      := TMemo.Create (fPanel);
  fMemo.Parent := fPanel;
  fMemo.OnKeyUp := @MemoKeyUp;
  fMyFace    := TImage.Create (fPanel);
  fMyFace.Parent := fPanel;
  fFriendFace := TImage.Create (fPanel);
  fFriendFace.Parent := fPanel;
  // Расположение
  fScroll.Align := alClient;
  fScroll.AutoScroll := True;
  fMyFace.Top := 4;
  fFriendFace.Top := 4;
  fMyFace.Left := 4;
  fFriendFace.Left := fPanel.Width - 70;
  fFriendFace.Width := 64;
  fFriendFace.Proportional := True;
  fFriendFace.Anchors := [akRight, akTop];
  fFriendFace.Stretch := True;
  fMyFace.Width := 64;
  fMyFace.Stretch := True;
  fMyFace.Proportional := True;
  fMemo.Left := 74;
  fMemo.Top  := 4;
  fMemo.Height := fPanel.Height - 8;
  fMemo.Width := fPanel.Width - 152;
  fMemo.Anchors := [akTop, akRight, akBottom, akLeft];
end;

function TCryptFrame.Add (UserName, szText :string; Date :TDate; Avatar :TPicture;
Files :TFiles) :boolean;
begin
  Result := True;
  // Выделяем память
  SetLength (fMsg, Length (fMsg) + 1);
  fmsg[High (fMsg)] := TMessages.Create (fScroll);
  fmsg[High (fMsg)].Parent := fScroll;
  // Выставляем позицию сообщения
  if High (fMsg) > 0 then
    fmsg[High (fMsg)].Top := fmsg[High (fMsg) - 1].Top + fmsg[High (fMsg) - 1].Height
  else
    fmsg[High (fMsg)].Top := 0;
  fmsg[High (fMsg)].Align := alTop;
  fmsg[High (fMsg)].ReAlign;
  // Содержимое диалога
  fMsg[High (fMsg)].Date := Date;
  fMsg[High (fMsg)].User := UserName;
  fMsg[High (fMsg)].Text := szText;
  fMsg[High (fMsg)].LoadImage (Avatar);
  fScroll.VertScrollBar.Position := fScroll.VertScrollBar.Range; // ? Ошибка ?
end;

procedure TCryptFrame.SetUserName (UserName :string);
begin
  fUserName := UserName;
end;

procedure TCryptFrame.SetFriendName (UserName :string);
begin
  fFriendName := UserName;
end;

procedure TCryptFrame.MemoKeyUp (Sender :TObject; var Key :word; Shift :TShiftState);
const
  VK_RETURN = 13;
begin
  if (ssCtrl in Shift) and (Key = VK_RETURN) then
  begin
    Add ('Антон', fMemo.Text, now, fMyFace.Picture, nil);
    fMsg[High (fMsg)].ReAlign;
    fMemo.Clear;
  end;
end;

function TCryptFrame.Count :integer;
begin
  Result := High (fMsg) + 1;
end;

procedure TCryptFrame.SetUserPic (FileName :string);
begin
  fMyFace.Picture.LoadFromFile (FileName);
end;

procedure TCryptFrame.SetFriendPic (FileName :string);
begin
  fFriendFace.Picture.LoadFromFile (FileName);
end;

procedure TCryptFrame.Debug;
begin
  ShowMessageFmt ('Range: %d \n Position: %d',
    [fScroll.VertScrollBar.Range, fScroll.VertScrollBar.Position]);
end;

end.
