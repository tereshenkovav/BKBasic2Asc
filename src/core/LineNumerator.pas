unit LineNumerator;

interface
uses Classes, Optional ;

type
  TLineNumerator = class
  private
    lines:TStringList ;
    errmsg:string ;
  public
    constructor Create(Alines:TStringList) ;
    function getNumeratedLines():TOptional<TStringList> ;
    function getErrMsg():string ;
  end;

implementation
uses SysUtils ;

{ TLineNumerator }

constructor TLineNumerator.Create(Alines: TStringList);
begin
  lines:=Alines ;
end;

function TLineNumerator.getErrMsg: string;
begin
  Result:=errmsg ;
end;

function TLineNumerator.getNumeratedLines: TOptional<TStringList>;
var s:string ;
    num:Integer ;
    newlines:TStringList ;
const STEP = 10 ;
begin
  newlines:=TStringList.Create ;
  num:=STEP ;
  for s in lines do begin
    newlines.Add(Format('%d %s',[num,s])) ;
    Inc(num,STEP) ;
  end;
  Result:=newlines ;
end;

end.
