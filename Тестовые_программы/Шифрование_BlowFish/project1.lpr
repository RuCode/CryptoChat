program project1;

uses
  BlowFish, Classes;

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

var
  Text, Password, Encoded: String;

begin
  // Получаем данные
  WriteLn('Демонстрация шифрования строки с помощью алгоритма BlowFish', #13+#10);
  Write('Введите текст: ');
  ReadLn(Text);
  Write('Введите пароль: ');
  ReadLn(Password);

  // Шифруем строку
  WriteLn(#13+#10, 'Шифруем текст');
  Encoded:= BlowFishEncryptStr(Text, Password);
  WriteLn(#13+#10, 'Результат: ', #13+#10, Encoded);

  // Дешифруем
  WriteLn(#13+#10, 'Дешифруем текст');
  Text:= BlowFishDecryptStr(Encoded, Password);
  WriteLn(#13+#10, 'Результат: ', #13+#10, Text);
  ReadLn;
end.

