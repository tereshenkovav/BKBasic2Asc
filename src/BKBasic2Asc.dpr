program BKBasic2Asc;

{$APPTYPE CONSOLE}

uses
  main ;

begin
  with TMain.Create() do begin
    Run() ;
    Free ;
  end;
end.

