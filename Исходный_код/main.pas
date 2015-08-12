unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  FrameLogin, FrameOfferRegisterUser,
  FrameRegisterUser, FrameDialogs, DataBases, Transports;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FrameLogin: TFrameLogin;
    FrameOfferRegister: TFrameOfferRegisterUser;
    FrameRegister: TFrameRegisterUser;
    FrameDialogs: TFrameWithDialogs;
    {: Показать фрэйм предложения регистрации пользователя}
    procedure ShowOfferRegisterFrame;
    {: Показать фрэйм регистрации пользователя}
    procedure ShowRegisterFrame;
    {: Показать фрэйм входа пользователя}
    procedure ShowLoginFrame;
    {: Показать фрэйм диалогов}
    procedure ShowFrameDialogs;
    {: Скрыть все фрэймы}
    procedure HideAllFrames;
    {: Установить заголовок основного окна}
    procedure SetCaptionWithForm(Frame: TFrame);
  public
    {: Восстановление системы}
    procedure LoadingSystem;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
// Запуск приложения
begin
  LoadingSystem;
end;

procedure TMainForm.ShowOfferRegisterFrame;
// Показать фрэйм регистрации пользователя
begin
  HideAllFrames;
  if not Assigned(FrameOfferRegister) then
  begin
    FrameOfferRegister := TFrameOfferRegisterUser.Create(MainForm);
    FrameOfferRegister.Align := alClient;
  end;
  FrameOfferRegister.Parent := MainForm;
  FrameOfferRegister.Visible := True;
  SetCaptionWithForm(FrameOfferRegister);
end;

procedure TMainForm.ShowRegisterFrame;
// Показать фрэйм регистрации пользователя
begin
  HideAllFrames;
  if not Assigned(FrameRegister) then
  begin
    FrameRegister := TFrameRegisterUser.Create(MainForm);
    FrameRegister.Align := alClient;
  end;
  FrameRegister.Parent := MainForm;
  FrameRegister.Visible := True;
  SetCaptionWithForm(FrameRegister);
end;

procedure TMainForm.ShowLoginFrame;
// Показать фрэйм входа в систему
begin
  HideAllFrames;
  if not Assigned(FrameLogin) then
  begin
    FrameLogin := TFrameLogin.Create(MainForm);
    FrameLogin.Align := alClient;
  end;
  FrameLogin.Parent := MainForm;
  FrameLogin.Visible := True;
  SetCaptionWithForm(FrameLogin);
end;

procedure TMainForm.ShowFrameDialogs;
// Показать фрэйм диалогов
var
  Info: TDataInfo;
begin
  HideAllFrames;
  if not Assigned(FrameDialogs) then
  begin
    FrameDialogs := TFrameWithDialogs.Create(MainForm);
    FrameDialogs.Align := alClient;
  end;
  FrameDialogs.LoadUsers; // Загружаем пользователей
  FrameDialogs.Parent := MainForm;
  FrameDialogs.Visible := True;
  SetCaptionWithForm(FrameDialogs);
  Info.Command := CMD_CONNECT;
  Transport.Enqueue(Info);
end;

procedure TMainForm.HideAllFrames;
// Скрыть все фрэймы
begin
  if Assigned(FrameOfferRegister) then
    FrameOfferRegister.Visible := False;
  if Assigned(FrameRegister) then
    FrameRegister.Visible := False;
  if Assigned(FrameLogin) then
    FrameLogin.Visible := False;
  if Assigned(FrameDialogs) then
    FrameDialogs.Visible := False;
end;

procedure TMainForm.SetCaptionWithForm(Frame: TFrame);
// Установить заголовок основного окна
begin
  Caption := 'CryptoChat: ' + Frame.Hint;
end;

procedure TMainForm.LoadingSystem;
// Восстановление всех настроек и параметров
begin
  // Если надо, генерируем новый путь к базе данных
  // Если бд нет на диске, то создаём
  if not FileExists(DataBase.FileName) then
    DataBase.CreateDataBase(DataBase.FileName)
  else
    DataBase.OpenDataBase(DataBase.FileName);
  // Узнаём количество пользователей, если их нет
  if (DataBase.GetUsersCount = INVALID_VALUE) or (DataBase.GetUsersCount = 0) then
    ShowOfferRegisterFrame // показываем предложение зарегистрироваться
  else
    ShowLoginFrame;// показываем предложение войти
end;

end.
