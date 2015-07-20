unit FrameDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, Graphics,
  ExtCtrls, Engine.DataBases;

type

  { TFrameWithDialogs }

  TFrameWithDialogs = class(TFrame)
    ImageList: TImageList;
    ListView: TListView;
    PageControl: TPageControl;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    procedure ListViewResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure LoadUsers;
  end;

implementation

{$R *.lfm}

{ TFrameWithDialogs }

procedure TFrameWithDialogs.ListViewResize(Sender: TObject);
begin
  ListView.Column[0].Width := ListView.Width - 14;
end;

procedure TFrameWithDialogs.LoadUsers;
var
  i: integer;
  Stream: TMemoryStream;
  BMP: TBitmap;
  JPG: TJPEGImage;
  Item: TListItem;
  Rect, ListItemRect: TRect;
  Buf: string;
begin
  BMP := TBitmap.Create;
  JPG := TJPEGImage.Create;
  Stream := TMemoryStream.Create;
  ListView.Clear;
  for i := 1 to DataBase.GetFriendsCount(DataBase.CurrentUserID) do
  begin
    try
      Stream.Clear;
      DataBase.SaveFriendAvatarToStream(DataBase.CurrentUserID, i, Stream);
      if Stream.Size > 0 then
      begin
        Stream.Seek(0, TSeekOrigin.soBeginning);
        Buf := '';
        Buf += char(AnsiChar(Stream.ReadByte));
        Buf += char(AnsiChar(Stream.ReadByte));
        Stream.Seek(0, TSeekOrigin.soBeginning);
        if AnsiLowerCase(Buf) = 'bm' then
        // Грузим BMP
          BMP.LoadFromStream(Stream, Stream.Size)
        else
        begin
          // Грузим JPG
          JPG.LoadFromStream(Stream);
          BMP.SetSize(22, 22);
          Rect.Left := 0;
          Rect.Top := 0;
          Rect.Right := JPG.Width;
          Rect.Bottom := JPG.Height;
          ListItemRect := Rect;
          ListItemRect.Right := 22;
          ListItemRect.Bottom := 22;
          BMP.Canvas.CopyRect(ListItemRect, JPG.Canvas, Rect);
        end;
        // PNG и ICO
        ImageList.Add(BMP, nil);
      end;
    except
    end;
    item := ListView.Items.Add;
    item.Caption := DataBase.GetUserNickName(i);
    item.ImageIndex := 1;
  end;
  Stream.Free;
  BMP.Free;
  JPG.Free;
end;

end.
