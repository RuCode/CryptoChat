unit BlowFishEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlowFish;

function BlowFishEncryptStr(const Src, AKey: string): string;
function BlowFishDecryptStr(const Src, AKey: string): string;

implementation

function BlowFishEncryptStr(const Src, AKey: string): string;
var
  VInput: TStringStream;
  VBF: TBlowFishEncryptStream;
begin
  VInput := TStringStream.Create('');
  VBF := TBlowFishEncryptStream.Create(AKey, VInput);
  try
    VBF.Write(Pointer(Src)^, Length(Src));
  finally
    VBF.Free;
    Result := VInput.DataString;
    VInput.Free;
  end;
end;

function BlowFishDecryptStr(const Src, AKey: string): string;
var
  VOutput: TStringStream;
  VBF: TBlowFishDeCryptStream;
begin
  VOutput := TStringStream.Create(Src);
  VBF := TBlowFishDeCryptStream.Create(AKey, VOutput);
  try
    SetLength(Result, VOutput.Size);
    VBF.Read(Pointer(Result)^, VOutput.Size);
  finally
    VBF.Free;
    VOutput.Free;
  end;
end;

end.



