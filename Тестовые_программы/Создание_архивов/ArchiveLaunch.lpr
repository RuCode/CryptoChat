program ArchiveLaunch;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Engine.Tar
  { you can add units after this };

var
  Tar: TCustomTar;
begin
  Tar:= TCustomTar.Create;
  Tar.AddFile('engine.tar.pas', 'engine.tar.pas');
  Tar.StoreToFile('test.tar');
  Tar.ExtractFile('engine.tar.pas', 'test.pas');
  Tar.Free;
  ReadLn;
end.

