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
uses SysUtils, Generics.Collections ;

const LABSYMS='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' ;

function isStrContainsOnlyChars(const str:string; const chars:string):Boolean ;
var c:char ;
begin
  for c in str do
    if not chars.Contains(c) then Exit(False) ;
  Result:=True ;
end;

{ TLineNumerator }

constructor TLineNumerator.Create(Alines: TStringList);
begin
  lines:=Alines ;
end;

function TLineNumerator.getErrMsg: string;
begin
  Result:=errmsg ;
end;

function ConvertLabelsByDict(str:string; labs:TDictionary<string,Integer>):string ;
var s:string ;
begin
  Result:='' ;
  for s in str.ToUpper().Trim().Split([',']) do
    if not labs.ContainsKey(s.Trim()) then
      raise Exception.Create('Not found label "'+s.Trim()+'" in labels table')
    else begin
      if Result<>'' then Result:=Result+',' ;
      Result:=Result+IntToStr(labs[s.Trim()]) ;
    end;
end ;

function ReplaceOneLabelFrom(str:string; labs:TDictionary<string,Integer>; p1,p2:Integer):string ;
begin
  Result:=str.Substring(0,p1)+
          ConvertLabelsByDict(str.Substring(p1,p2-p1+1),labs)+
           str.Substring(p2) ;
end;

function ReplaceTwoLabelFrom(str:string; labs:TDictionary<string,Integer>; p1,p2,p3,p4:Integer):string ;
begin
  Result:=str.Substring(0,p1)+
          ConvertLabelsByDict(str.Substring(p1,p2-p1+1),labs)+
          str.Substring(p2,p3-p2)+
          ConvertLabelsByDict(str.Substring(p3,p4-p3+1),labs)+
          str.Substring(p4) ;
end ;

function TLineNumerator.getNumeratedLines: TOptional<TStringList>;
var s,str,lab,cmd:string ;
    p,i,num:Integer ;
    newlines:TStringList ;
    labs:TDictionary<string,Integer> ;
    goidx:TList<Integer> ;
    elseidx:Integer ;
const STEP = 10 ;
begin
  newlines:=TStringList.Create ;
  labs:=TDictionary<string,Integer>.Create ;
  num:=STEP ;
  i:=0 ;
  while i<lines.Count do begin
    cmd:=lines[i].Trim() ;
    p:=lines[i].IndexOf(':') ;
    if p<>-1 then begin
      lab:=lines[i].Substring(0,p).ToUpper().Trim() ;
      if isStrContainsOnlyChars(lab,LABSYMS) then begin
        labs.Add(lab,num) ;
        cmd:=lines[i].Remove(0,p+1).Trim() ;
        if cmd='' then begin
          Inc(i) ;
          if i<lines.Count then cmd:=lines[i].Trim() ;
        end ;
      end ;
    end ;
    newlines.Add(Format('%d %s',[num,cmd])) ;
    Inc(num,STEP) ;
    Inc(i) ;
  end;

  for i := 0 to newlines.Count-1 do begin
    str:=newlines[i] ;
    goidx:=TList<Integer>.Create ;
    elseidx:=-1 ;
    for p := 0 to str.Length-1 do begin
      if str.ToUpper().Substring(p,6)=' GOTO ' then goidx.Add(p+6) ;
      if str.ToUpper().Substring(p,7)=' GOSUB ' then goidx.Add(p+7) ;
      if str.ToUpper().Substring(p,9)=' RESTORE ' then goidx.Add(p+9) ;
      if str.ToUpper().Substring(p,6)=' ELSE ' then elseidx:=p ;
    end ;
    {
    Четыре варианта
    
    goidx.Count=1, elseidx=-1
    парсим от goidx до конца строки

    goidx.Count=1, elseidx<>-1
    if elseidx<goidx то парсим от goidx до конца строки, иначе от goidx до elseidx
       
    goidx.Count=2, elseidx<>-1
    парсим от goidx[1] до конца строки и от goidx[0] до elseidx
    }
    if goidx.Count=1 then begin
      if elseidx<goidx[0] then 
        str:=ReplaceOneLabelFrom(str,labs,goidx[0],str.Length) 
      else  
        str:=ReplaceOneLabelFrom(str,labs,goidx[0],elseidx) ;
    end
    else 
    if goidx.Count=2 then begin
      str:=ReplaceTwoLabelFrom(str,labs,goidx[0],elseidx,goidx[1],str.Length) ;
    end ;
    goidx.Free ;

    newlines[i]:=str ;
  end;

  labs.Free ;
  Result:=newlines ;
end;

end.
