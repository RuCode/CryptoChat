unit Transports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Mails, Databases, SpecializedQueue;

const
  {:Подключиться к первому транспорту}
  CMD_CONNECT = 1;
  {:Отключиться от транспорта}
  CMD_DISCONNECT = 2;

type

  { TTransport }

  TTransport = class(TThread)
  private
    Mail: TMail;
    Queue: TQueueInteger;
    TransportType: integer;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    procedure Enqueue(Command: integer);
  end;

var
  Transport: TTransport;

implementation

{ TTransport }

constructor TTransport.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  Queue := TQueueInteger.Create;
  inherited Create(CreateSuspended);
end;

destructor TTransport.Destroy;
begin
  Queue.Free;
  inherited Destroy;
end;

procedure TTransport.Execute;
begin
  // Основной цикл сетевого транспорта
  while not Terminated do
  begin
    if Queue.Count <= 0 then
    begin
      if TransportType = CONNECTIONTYPE_EMAIL then
        // Mail.nop;
      ;
    end
    else
      case Queue.Dequeue of
        // Подключиться
        CMD_CONNECTION:
        begin
          TransportType := Database.GetTransportType(DataBase.CurrentUserID);
          if TransportType = CONNECTIONTYPE_EMAIL then
          begin
            Mail := TMail.Create;
            Mail.IMAPHost := DataBase.GetTransportHostIn(DataBase.CurrentUserID);
            Mail.IMAPPort := DataBase.GetTransportPortIn(DataBase.CurrentUserID);
            Mail.SMTPHost := DataBase.GetTransportHostOut(DataBase.CurrentUserID);
            Mail.SMTPPort := DataBase.GetTransportPortOut(DataBase.CurrentUserID);
            Mail.UserName := DataBase.GetTransportUserName(DataBase.CurrentUserID);
            Mail.Password := DataBase.GetTransportPassword(DataBase.CurrentUserID);
            Mail.Connected := True;
          end;
        end;
        // Отключаемся
        CMD_DISCONNECT:
        begin
          if TransportType = CONNECTIONTYPE_EMAIL then
            Mail.Free;
        end;
          // Не смогли определить комманду
        else
          raise Exception.Create('Не известная комманда');
      end;
  end;
  // Выходим из потока
  if TransportType = CONNECTIONTYPE_EMAIL then
    Mail.Free;
end;

procedure TTransport.Enqueue(Command: integer);
// Положить комманду
begin
  Queue.Enqueue(Command);
end;

initialization
  Transport := TTransport.Create(False);

finalization
  Transport.Terminate;

end.
