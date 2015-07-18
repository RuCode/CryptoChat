unit ContactBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Graphics, LCLType;

type

  { TContactBox }

  TContactBox = class(TListBox)
  private
    fMsgCount: array of integer;
    procedure ResizeArray;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure ListBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState); virtual;
  public
    procedure NewMessage(fItem: integer);
    procedure ClearMessage(fItem: integer);
  end;

implementation

{ TContactBox }

procedure TContactBox.ResizeArray;
var
  OldLen: integer;
  i:      integer;
begin
  OldLen := High(fMsgCount) + 1;
  if OldLen <> Items.Count then
  begin
    SetLength(fMsgCount, Items.Count);
    for i := OldLen to High(fMsgCount) do
      fMsgCount[i] := 0;
  end;
end;

constructor TContactBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Style      := lbVirtual;
  OnDrawItem := @ListBoxDrawItem;
  SetLength(fMsgCount, 1);
end;

procedure TContactBox.ListBoxDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
var
  BufStr: string;
begin
  with (Control as TContactBox).Canvas do
  begin
{   Color:= clWhite;
    if  (odSelected in State) then
    begin
      Brush.Color:=clBlack;
      Font.Color := clWhite
    end
    else
      Font.Color := clBlack;    }
    FillRect(aRect);
    Pen.Style   := psClear;
    Brush.Style := bsClear;
    BufStr      := '';
    if fMsgCount[Index] <> 0 then
      BufStr := '(' + IntToStr(fMsgCount[Index]) + ') ';
    BufStr   += (Control as TListBox).Items[Index];
    TextOut(aRect.Left + 2, aRect.Top + 2, BufStr);
    if (odFocused in State) then
      DrawFocusRect(aRect);
  end;
end;

procedure TContactBox.NewMessage(fItem: integer);
begin
  ResizeArray;
  fMsgCount[fItem] += 1;
end;

procedure TContactBox.ClearMessage(fItem: integer);
begin
  ResizeArray;
  fMsgCount[fItem] := 0;
end;

end.

