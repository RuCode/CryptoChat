unit FrameDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, Graphics, Dialogs,
  ExtCtrls, DataBases, AddFriendDlg, Transports;

type

  { TFrameWithDialogs }

  TFrameWithDialogs = class(TFrame)
    ImageList: TImageList;
    ListView: TListView;
    PageControl: TPageControl;
    PanelLeft: TPanel;
    PanelLeftBottom: TPanel;
    ProgressBar: TProgressBar;
    Splitter: TSplitter;
    MainStatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    ToolBar: TToolBar;
    ToolButtonDel: TToolButton;
    ToolButtonAdd: TToolButton;
    procedure ListViewResize(Sender: TObject);
    procedure MainStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure ToolButtonAddClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure LoadFriendsFromDB;
    procedure ShowWaitForm;
    procedure HideWaitForm;

    procedure OnStartOperation(AName: string);
    procedure OnEndOperation;
  end;

implementation

{$R *.lfm}

uses WaitForm;

var
  FormWait: TFormWait;

{ TFrameWithDialogs }

procedure TFrameWithDialogs.ListViewResize(Sender: TObject);
begin
  ListView.Column[0].Width := ListView.Width - 14;
end;

procedure TFrameWithDialogs.MainStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
// Должен рисовать прогресс бар в статус баре
begin
  case Panel.Index of
    0:
    begin
      ProgressBar.BoundsRect := Rect;
      ProgressBar.PaintTo(StatusBar.Canvas.Handle, Rect.Left, Rect.Top);
    end;
  end;
end;

procedure TFrameWithDialogs.ToolButtonAddClick(Sender: TObject);
// Добавляем нового друга
begin
  with FormAddFriend do
  begin
    Clear;
    if ShowModal = mrOk then
      if DataBase.AddFriend(DataBase.CurrentUserID, EditName.Text, EditMail.Text, OpenPictureDialog.FileName) then
        Transport.AddFriend(EditName.Text, EditMail.Text, OpenPictureDialog.FileName);
  end;
end;

constructor TFrameWithDialogs.Create(AOwner: TComponent);
  // Если открывается этот фрэйм, значит получаем все сообщения
begin
  inherited Create(AOwner);
  // Ставим иконки
  ToolButtonAdd.ImageIndex := 0;
  ToolButtonDel.ImageIndex := 1;
  // Установим новые события
  Transport.OnStartOperation := @OnStartOperation;
  Transport.OnEndOperation := @OnEndOperation;
  // Впишем прогресс бар в статус бар
  ProgressBar.Width := 100;
  ProgressBar.Parent := self;
  ProgressBar.Visible := False;
  ProgressBar.Position := 100;

  // Грузим список друзей
  LoadFriendsFromDB;
  // Читаем входящие сообщения
  Transport.SynchronizeMails;
end;

procedure TFrameWithDialogs.LoadFriendsFromDB;
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

procedure TFrameWithDialogs.ShowWaitForm;
// Показать диалог ожидания
begin
  if Assigned(FormWait) then
    exit;
  FormWait := TFormWait.Create(Self);
  FormWait.ShowModal;
end;

procedure TFrameWithDialogs.HideWaitForm;
// Скрыть диалог ожидания
begin
  if not Assigned(FormWait) then
    Exit;
  FormWait.Close;
  MessageDlg('Информация', Format('На почту %s отправлен запрос открытого ключа, после получения ответа, Вы можете начать переписку...',
    [FormAddFriend.EditMail.Text]), mtInformation, [mbOK], '');
end;

procedure TFrameWithDialogs.OnStartOperation(AName: string);
// Устанавливаем статус операции и дёргаем ProgressBar
begin
  MainStatusBar.Panels[1].Text := AName;
  ProgressBar.Position := 100;
end;

procedure TFrameWithDialogs.OnEndOperation;
// Перестаем дёрьгать ProgressBar
begin
  MainStatusBar.Panels[1].Text := 'Все задания выполнены...';
end;

end.
