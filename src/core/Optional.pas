unit Optional;

///  Файл взят из библиотеки TAVPascalLib, коммит 8145b050

{
   Реализация структуры-аналога std::nullopt в C++
}

interface

type
  TOptional<T> = record
  private
    b:Boolean ;
    v:T ;
  public
    class operator Implicit(a:TOptional<T>):Boolean  ; overload ;
    class operator Implicit(a:T):TOptional<T> ; overload ;
    {
    Это не будет работать в FPC, есть баг
    https://gitlab.com/freepascal.org/fpc/source/-/issues/40256
    так что для FPC вместо прямого преобразования всегда используем Value
    }
    {$ifndef fpc}
    class operator Implicit(a:TOptional<T>):T  ; overload ;
    {$endif}
    function Value():T ;
    function IsOk():Boolean ;
    class function NewOptional(value:T):TOptional<T> ; static ;
    class function NullOptional():TOptional<T> ; static ;
  end;

implementation
uses SysUtils ;

{ TOptional }

class operator TOptional<T>.Implicit(a: TOptional<T>): Boolean;
begin
  Result:=a.b ;
end;

{$ifndef fpc}
class operator TOptional<T>.Implicit(a: TOptional<T>): T;
begin
  Result:=a.Value() ;
end;
{$endif}

class operator TOptional<T>.Implicit(a: T): TOptional<T>;
begin
  Result:=NewOptional(a) ;
end;

function TOptional<T>.IsOk: Boolean;
begin
  Result:=b ;
end;

class function TOptional<T>.NewOptional(value: T): TOptional<T>;
begin
  Result.b:=True ;
  Result.v:=value ;
end;

class function TOptional<T>.NullOptional: TOptional<T>;
begin
  Result.b:=False ;
  Result.v:=default(T) ;
end;

function TOptional<T>.Value: T;
begin
  if b then Result:=v else raise Exception.Create
    ('Cant get undefined value of TOptional<T>');
end;

end.
