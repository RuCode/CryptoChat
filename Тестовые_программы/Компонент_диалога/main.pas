unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, DialogWidgets;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Message: TDialogMessage;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  Pic: TPicture;
begin
  Message := TDialogMessage.Create(Self);
  Message.Left := 0;
  Message.Top := 0;
  Message.Width := 400;
  Message.Height := 80;
  Message.Parent := Form1;
  Message.Align := alTop;
  // заполняем
  Message.Establish('Вася', 'Здарова чувак!!!', now);
  Pic := TPicture.Create;
  Pic.LoadFromFile('1.jpg');
  Message.Picture := pic;
  Pic.Free;
  Message.AddAttachInfo('Инструкция.pdf', nil);
end;

end.

