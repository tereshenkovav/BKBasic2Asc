program BKBasic2Asc;

{$APPTYPE CONSOLE}

uses
  main,
  LineNumerator in 'core\LineNumerator.pas',
  Optional in 'core\Optional.pas';

begin
  with TMain.Create() do begin
    Run() ;
    Free ;
  end;
end.

