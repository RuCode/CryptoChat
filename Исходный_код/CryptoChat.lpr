program CryptoChat;

{$mode objfpc}{$H+}

{ $DEFINE UseCThreads} // Чёта не работает

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  pl_richmemo,
  Main,
  SQLite3,
  SQLite3Utils,
  SQLite3Wrap,
  FrameLogin,
  FrameRegisterUser,
  FrameOfferRegisterUser,
  engine.databases,
  FrameDialogs,
  AddFriendDlg,
  Engine.Transport;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormAddFriend, FormAddFriend);
  Application.Run;
end.
