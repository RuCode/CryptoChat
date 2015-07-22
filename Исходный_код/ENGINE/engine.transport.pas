unit Engine.Transport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { TTransport }

  TTransport = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;

var
  Transport: TTransport;

implementation

{ TTransport }

procedure TTransport.Execute;
begin
  Sleep(100);
end;

constructor TTransport.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

//initialization
  //Transport := TTransport.Create(False);

  //finalization
  //Transport.Free;

end.

