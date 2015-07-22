unit FrameDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, Graphics, Dialogs,
  ExtCtrls, Engine.DataBases, RichMemo, AddFriendDlg;

type

  { TFrameWithDialogs }

  TFrameWithDialogs = class(TFrame)
    ImageList: TImageList;
    ListView: TListView;
    PageControl: TPageControl;
    PanelLeft: TPanel;
    PanelLeftBottom: TPanel;
    RichMemo: TRichMemo;
    Splitter: TSplitter;
    TabSheet1: TTabSheet;
    ToolBar: TToolBar;
    ToolButtonAdd: TToolButton;
    procedure ListViewResize(Sender: TObject);
    procedure ToolButtonAddClick(Sender: TObject);
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

procedure TFrameWithDialogs.ToolButtonAddClick(Sender: TObject);
// Добавляем нового друга
begin
  with FormAddFriend do
  begin
    Clear;
    if ShowModal = mrOk then
      if DataBase.AddFriend(DataBase.CurrentUserID, EditName.Text, EditMail.Text, OpenPictureDialog.FileName) then
        MessageDlg('Информация', Format('На почту %s отправлен запрос открытого ключа, после получения ответа, Вы можете начать переписку...',
          [EditMail.Text]), mtInformation, [mbOK], '');
  end;
end;

procedure TFrameWithDialogs.LoadUsers;
// Загружаем пользователей из БД
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
    item.Caption := DataBase.GetFriendNickName(DataBase.CurrentUserID, i);
    item.ImageIndex := 1;
  end;
  Stream.Free;
  BMP.Free;
  JPG.Free;
end;

end.
