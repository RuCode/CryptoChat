unit WaitForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls;

type

  { TFormWait }

  TFormWait = class(TForm)
    ProgressBar: TProgressBar;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormWait: TFormWait;

implementation

{$R *.lfm}

{ TFormWait }

procedure TFormWait.TimerTimer(Sender: TObject);
begin
  if ProgressBar.Position = ProgressBar.Max then
    ProgressBar.Position := ProgressBar.Min;
  ProgressBar.Position := ProgressBar.Position + 1;
end;

end.

