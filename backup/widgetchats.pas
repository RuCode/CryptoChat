unit WidgetChats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CryptChat, Dialogs;

type

  { TCryptChat }

  TCryptChat = class(TCustomCryptChat)
  public
    procedure GlobalUpdate; override;
  published
    property HostPop3;
    property HostSmtp;
    property Host;
  end;

implementation

{ TCryptChat }

procedure TCryptChat.GlobalUpdate;
const
  // Сообщения всегда имеют эту тему
  CHAT_MAIN_SUBJECT       = '{6D78D26F-347E-4D57-9E9C-03C82139CD38}';
var
  i: Integer;
begin
  inherited GlobalUpdate;
  host:= 'pop.yandex.ru';
  if not ConnectMail then ShowMessage('No connect');
  for i:= 1 to GetMailCount-1 do
      ShowMessage(GetMail(i));
  {
      if GetMailSubject(i) = CHAT_MAIN_SUBJECT then
      begin
        // processing
        ShowMessage(GetMailFrom(i)+' - '+InTToStr(GetAttachCount(i)));//GetMailCommand(i));
      end;}
end;

end.

