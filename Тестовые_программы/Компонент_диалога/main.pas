unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, DialogWidgets, DialogItems;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Message: TDialogItem;
    Dialog: TDialog;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
//var
//  i: integer;
begin
  // Одно сообщение
 { Message := TDialogMessage.Create(Self);
  Message.Left := 0;
  Message.Top := 0;
  Message.Width := 400;
  Message.Height := 80;
  Message.Parent := Form1;
  Message.Align := alTop;
  // заполняем
  Message.Establish('Вася', 'Здарова чувак!!!', now);
  Message.Picture.LoadFromFile('1.jpg');
  Message.AddAttachInfo('Инструкция.pdf', nil);}

  // Диалог
  Dialog := TDialog.Create(self);
  Dialog.Left := 0;
  Dialog.Top := 100;
  Dialog.Width := 400;
  Dialog.Height := 80;
  Dialog.Parent := Form1;
  Dialog.Align := alClient;
  Dialog.UserName := 'Антон';
  Dialog.FriendName := 'Сергей';
  Dialog.UserPicture.LoadFromFile('1.jpg');
  Dialog.FriendPicture.LoadFromFile('2.png');


  Dialog.Add('Здарова мужик!!!' + #13 + #10 + 'Ну это проверка текста, ведь диалоги должны быть разной длинны', Now, True);
  Dialog.Add('Здарова мужик!!!', Now, True);
  Dialog.Add('Здарова мужик!!!', Now, True);

 { for i := 1 to 100 do
  begin
    Dialog.Add('Здарова мужик!!!', Now, True);
    Dialog.Add('Здарова мужик 22222!!!Здарова мужик 22222!!!', Now, False);
  end;
  Dialog.Items[Dialog.Count - 1].AddAttachInfo('Test.txt');}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Message.Free;
  Dialog.Free;
end;

end.

