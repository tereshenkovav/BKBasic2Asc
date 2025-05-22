unit main ;

interface
uses SysUtils, Classes ;

type
  TMain = class
  private
    procedure ExitWithError(const msg: string; code: Integer);
  public
    procedure Run() ;
  end;

implementation
uses Generics.Collections, Math ;

const MAINHELP = 'Usage: Basic_filename Asc_filename [parameters]'#13#10+
  'Parameters:'#13#10+
  '/basiccodepage=utf8|win1251|koi8r|oem866 - convert Basic file from codepage' ;

ASC_NAME_LENGTH = 6 ;
HEADER_SIZE = 4 ;
MAX_BLOCK_SIZE = 256 ;

procedure TMain.ExitWithError(const msg: string; code: Integer);
begin
  Writeln(msg) ;
  Halt(code) ;
end;

procedure TMain.Run() ;
var script:TStringList ;
    s,pname:string ;
    i,j,p,filecnt,blocksize:Integer ;
    srcenc,destfile,binfile:string ;
    data:TList<Byte> ;
    stm:TFileStream ;
    buf:TBytes ;
begin
  try
    if ParamCount<1 then ExitWithError(MAINHELP,1) ;

    srcenc:='utf8' ;
    for i := 3 to ParamCount do begin
      if ParamStr(i)[1]<>'/' then ExitWithError('Unknown argument: '+ParamStr(i)+', use /name=value',2) ;
      p:=ParamStr(i).IndexOf('=') ;
      if p=-1 then ExitWithError('Unknown argument: '+ParamStr(i)+', use /name=value',3) ;
      pname:=ParamStr(i).Substring(1,p-1) ;
      if pname='basiccodepage' then begin
        srcenc:=ParamStr(i).Substring(p+1).ToLower() ;
        if (srcenc<>'utf8') and (srcenc<>'win1251') and
           (srcenc<>'koi8r') and (srcenc<>'oem866')  then
          ExitWithError('Unknown basiccodepage: '+pname,4) ;
      end
      else
        ExitWithError('Unknown parameter: '+pname,5) ;
    end;

    if not FileExists(ParamStr(1)) then
      ExitWithError('Not found Basic file: '+ParamStr(1),6) ;

    destfile:=ParamStr(2) ;
    if Length(ExtractFileName(destfile))>ASC_NAME_LENGTH then
      ExitWithError('Too long Asc filename (max '+
      IntToStr(ASC_NAME_LENGTH)+' chars): '+ParamStr(2),7) ;

    destfile:=ParamStr(2)+StringOfChar(' ',ASC_NAME_LENGTH-Length(ExtractFileName(destfile))) ;

    Writeln('Reading Basic file...') ;
    script:=TStringList.Create() ;
    script.LoadFromFile(ParamStr(1),TEncoding.UTF8) ;
    data:=TList<Byte>.Create() ;
    for s in script do begin
      if s.Trim().Length=0 then Continue ;
      if s.Trim().StartsWith('''') then Continue ;
      data.AddRange(TEncoding.UTF8.GetBytes(s.Trim())) ;
      data.Add($0A) ;
    end;
    data.Add($1A) ;
    script.Free ;

    Writeln('Writeln Asc files...') ;

    if not destfile.ToLower().EndsWith('.asc') then destfile:=destfile+'.asc' ;

    filecnt:=data.Count div MAX_BLOCK_SIZE ;
    if data.Count mod MAX_BLOCK_SIZE<>0 then Inc(filecnt) ;

    for i := 0 to filecnt-1 do begin
      binfile:=Format('%s #%.3d.bin',[destfile,i]) ;
      if FileExists(binfile) then SysUtils.DeleteFile(binfile) ;

      blocksize:=IfThen(i=filecnt-1,data.Count-(filecnt-1)*MAX_BLOCK_SIZE,MAX_BLOCK_SIZE) ;
      SetLength(buf,blocksize+HEADER_SIZE) ;
      buf[0]:=$EE ;
      buf[1]:=$3D ;
      buf[2]:=Byte(blocksize mod MAX_BLOCK_SIZE) ;
      buf[3]:=Byte(blocksize div MAX_BLOCK_SIZE) ;
      for j := 0 to blocksize-1 do
        buf[HEADER_SIZE+j]:=data[i*MAX_BLOCK_SIZE+j] ;
      stm:=TFileStream.Create(binfile,fmCreate) ;
      stm.WriteBuffer(buf[0],Length(buf)) ;
      stm.Free ;

    end;

    binfile:=destfile+'.bin' ;
    if FileExists(binfile) then SysUtils.DeleteFile(binfile) ;

    stm:=TFileStream.Create(binfile,fmCreate) ;
    stm.Free ;

    data.Free ;

  except
    on E: Exception do
      Writeln('Error '+E.ClassName+': '+E.Message);
  end;
end ;

end.
