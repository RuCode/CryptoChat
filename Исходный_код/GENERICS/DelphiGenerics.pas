unit DelphiGenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TQueue }

  generic TQueue<T> = class
    Items: array of T;
    {: Положить в очередь}
    procedure Enqueue(Value: T);
    {: Взять из очереди}
    function Dequeue: T;
    {: Количество элементов}
    function Count: integer;
  end;

implementation

{ TQueue }

procedure TQueue.Enqueue(Value: T);
begin
  SetLength(Items, Length(Items) + 1);
  Items[Length(Items) - 1] := Value;
end;

function TQueue.Dequeue: T;
var
  i: integer;
begin
  Result := Items[0];
  for i := Low(Items) to High(Items) - 1 do
    Items[i] := Items[i + 1];
  SetLength(Items, Length(Items) - 1);
end;

function TQueue.Count: integer;
begin
  Result := Length(Items);
end;

end.


