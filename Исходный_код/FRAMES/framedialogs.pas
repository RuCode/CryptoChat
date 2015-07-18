unit FrameDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  { TFrameWithDialogs }

  TFrameWithDialogs = class(TFrame)
    Button1: TButton;
    procedure FrameClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TFrameWithDialogs }

procedure TFrameWithDialogs.FrameClick(Sender: TObject);
begin

end;

end.

