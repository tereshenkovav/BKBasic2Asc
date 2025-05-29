unit Optional;

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
    ��� �� ����� �������� � FPC, ���� ���
    https://gitlab.com/freepascal.org/fpc/source/-/issues/40256
    ��� ��� ��� FPC ������ ������� �������������� ������ ���������� Value
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
