unit FrameDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls;

type

  { TFrameWithDialogs }

  TFrameWithDialogs = class(TFrame)
    Button1: TButton;
    ListView: TListView;
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TFrameWithDialogs }

procedure TFrameWithDialogs.FrameResize(Sender: TObject);
begin
  ListView.Column[0].Width := ListView.Width - 14;
end;

end.

