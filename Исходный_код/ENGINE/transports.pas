unit Transports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Mails, Databases, DelphiGenerics, RsaEx, Tars, GUIDCommands;

const
  {:Подключиться к первому транспорту}
  CMD_CONNECT = 1;
  {:Отключиться от транспорта}
  CMD_DISCONNECT = 2;
  {:Отправить запрос на добавления в друзья}
  CMD_ADDFRIEND = 3;
  {:Синхронизация сырых сообщений, нужна для анализа ссылочной целостности и получения новых сообщений на лету}
  CMD_SYNCH_RAWMESSAGES = 4;

type
  TDataInfo = record
    case Command: integer of
      CMD_CONNECT: ();
      CMD_DISCONNECT: ();
      CMD_ADDFRIEND: (
        Name,
        Email,
        AvatarPath: ShortString;
        OnEndOperation: TProcedureOfObject);
      CMD_SYNCH_RAWMESSAGES: ();
  end;

type
  TDataQueue = specialize TQueue<TDataInfo>;
  {
    Когда операция начинается и заканчивается, сообщаем главному окну об этом.
  }
  TOnStartOperation = procedure(AName: string) of object;
  TOnEndOperation = TProcedureOfObject;

  { TTransport }

  TTransport = class(TThread)
  private
    FOnEndOperation: TProcedureOfObject;
    FOnStartOperation: TOnStartOperation;
    Mail: TMail;
    fQueue: TDataQueue;
    TransportType: integer;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    procedure Enqueue(Command: TDataInfo);
  public
    // Не удобно использовать Enqueue, описываем операции тут
    procedure Connect;
    procedure Disconnect;
    procedure AddFriend(AName, AEmail, AAvatarPath: ShortString);
    procedure SynchronizeMails;
    // События
    property OnStartOperation: TOnStartOperation read FOnStartOperation write FOnStartOperation;
    property OnEndOperation: TProcedureOfObject read FOnEndOperation write FOnEndOperation;
  end;

var
  Transport: TTransport;

implementation

{ TTransport }

constructor TTransport.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  Mail := TMail.Create;
  fQueue := TDataQueue.Create;
  inherited Create(CreateSuspended);
end;

destructor TTransport.Destroy;
begin
  Mail.Free;
  fQueue.Free;
  inherited Destroy;
end;

procedure TTransport.Execute;
var
  Info: TDataInfo;
  Rsa: TRsa;
  Tar: TTar;
  Stream: TMemoryStream;
  i: integer;
begin
  // Основной цикл сетевого транспорта
  while not Terminated do
  begin
    if fQueue.Count <= 0 then
    begin
      if TransportType = CONNECTIONTYPE_EMAIL then
      begin
        Sleep(100);
        if Assigned(Mail) and Mail.Connected then
          Mail.nop;
      end;
    end
    else
    begin
      info := fQueue.Dequeue;
      case Info.Command of
        // Подключиться
        CMD_CONNECT:
        begin
          if Assigned(FOnStartOperation) then
            OnStartOperation('Производим попытку подключения...');
          TransportType := Database.GetTransportType(DataBase.CurrentUserID);
          if TransportType = CONNECTIONTYPE_EMAIL then
          begin
            Mail.IMAPHost := DataBase.GetTransportHostIn(DataBase.CurrentUserID);
            Mail.IMAPPort := DataBase.GetTransportPortIn(DataBase.CurrentUserID);
            Mail.SMTPHost := DataBase.GetTransportHostOut(DataBase.CurrentUserID);
            Mail.SMTPPort := DataBase.GetTransportPortOut(DataBase.CurrentUserID);
            Mail.UserName := DataBase.GetTransportUserName(DataBase.CurrentUserID);
            Mail.Password := DataBase.GetTransportPassword(DataBase.CurrentUserID);
            Mail.Connected := True;
          end;
          if Assigned(FOnEndOperation) then
            OnEndOperation();
        end;
        // Отключаемся
        CMD_DISCONNECT:
        begin
          if Assigned(FOnStartOperation) then
            OnStartOperation('Отключаемся от сервера...');
          if TransportType = CONNECTIONTYPE_EMAIL then
            Mail.Connected := False;
          if Assigned(FOnEndOperation) then
            OnEndOperation();
        end;
        // Добавляем в друзья
        CMD_ADDFRIEND:
        begin
          // Генерируем ключи
          if Assigned(FOnStartOperation) then
            OnStartOperation('Генерируем ключи...');
          Rsa := TRSA.Create;
          Rsa.GenKeys;
          // Пихаем в тар и отправляем на мыло
          if Assigned(FOnStartOperation) then
            OnStartOperation('Отправляем сообщение с открытым ключем...');
          Tar := TTar.Create;
          Tar.AddTextFile('command.cfg', CC_IWONTADDFRIEND);
          Tar.AddTextFile('public.pem', Rsa.PublicKey);
          Stream := TMemoryStream.Create;
          Tar.StoreToStream(TStream(Stream));
          Mail.SendMail(Info.Email, CC_ALLMAILSSUBJECT, '...', CC_ATTACHNAME, Stream);
          // Смываем за собой
          Rsa.Free;
          Tar.Free;
          Stream.Free;
          if Assigned(FOnEndOperation) then
            OnEndOperation();
        end;
        // Синхронизация сообщений
        CMD_SYNCH_RAWMESSAGES:
        begin
          if Assigned(FOnStartOperation) then
            OnStartOperation('Синхронизируем сообщения...');
          { TODO : Нужно сделать фичу синхронизации }
          if mail.Connected then
            for i := 0 to mail.CountOfMails do
              mail.GetMailHeader(i);

          if Assigned(FOnEndOperation) then
            OnEndOperation();
        end
          // Не смогли определить комманду
        else
          raise Exception.Create('Не известная комманда');
      end;
    end;
  end;
  // Выходим из потока
  if TransportType = CONNECTIONTYPE_EMAIL then
    Mail.Free;
end;

procedure TTransport.Enqueue(Command: TDataInfo);
// Положить комманду
begin
  fQueue.Enqueue(Command);
end;

procedure TTransport.Connect;
// Подключение к серверу
var
  Info: TDataInfo;
begin
  Info.Command := CMD_CONNECT;
  Enqueue(Info);
end;

procedure TTransport.Disconnect;
// Отключение от сервера
var
  Info: TDataInfo;
begin
  Info.Command := CMD_DISCONNECT;
  Enqueue(Info);
end;

procedure TTransport.AddFriend(AName, AEmail, AAvatarPath: ShortString);
// Добавление нового друга
var
  Info: TDataInfo;
begin
  Info.Command := CMD_ADDFRIEND;
  Info.Name := AName;
  Info.Email := AEmail;
  Info.AvatarPath := AAvatarPath;
  Enqueue(Info);
end;

procedure TTransport.SynchronizeMails;
// Синхронизируем почту
begin

end;

initialization
  Transport := TTransport.Create(False);

finalization
  Transport.Terminate;

end.
