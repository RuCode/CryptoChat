unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  VirtualTrees;

type
  PVSTRecord = ^TVSTRecord;

  TVSTRecord = record
    ElementName: string;
    ElementNumber: integer;
  end;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ImageList1: TImageList;
    VST: TVirtualStringTree;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PVSTRecord;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    CellText := Data^.ElementName + #13 + #10 + IntToStr(Data^.ElementNumber);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  VST.NodeDataSize := SizeOf(TVSTRecord);
end;

procedure TForm1.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin

end;

procedure TForm1.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PVSTRecord;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

procedure TForm1.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var ImageIndex: Integer);
begin
  ImageIndex:= Random(10);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  RootNode, ChildNode: PVirtualNode;
  I: integer;
  Data: PVSTRecord;
begin
  RootNode := VST.AddChild(VST.RootNode);
  VST.ReinitNode(RootNode, False);
  Data := VST.GetNodeData(RootNode);
  Data^.ElementName := 'Корневой элемент';
  Data^.ElementNumber := 0;
  RootNode := VST.AddChild(VST.RootNode);
  if not (vsInitialized in RootNode^.States) then
    VST.ReinitNode(RootNode, False);
  Data := VST.GetNodeData(RootNode);
  Data^.ElementName := 'Корневой элемент';
  Data^.ElementNumber := 1;
 { for I := 1 to 10 do
  begin
    ChildNode := VST.AddChild(RootNode);
    if not (vsInitialized in ChildNode^.States) then
      VST.ReinitNode(ChildNode, False);
    Data := VST.GetNodeData(ChildNode);
    Data^.ElementName := 'Дочерний элемент';
    Data^.ElementNumber := I;
  end;}
end;

procedure TForm1.VSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  InitialStates := [ivsMultiline, ivsSelected];
end;

end.


