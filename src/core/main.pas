unit main ;

interface
uses SysUtils, Classes ;

type
  TDefBlockState = (dbsNone,dbsThen,dbsElse) ;
  TDefBlockCommand = (dbcNone,dbcDefine,dbcElse,dbcEnd) ;

  TMain = class
  private
    function StripCommentFromLine(const line:string):string ;
    function GetDefineCommandFromLine(const line:string; var defname:string):TDefBlockCommand ;
    procedure ExitWithError(const msg: string; code: Integer);
  public
    procedure Run() ;
  end;

implementation
uses Generics.Collections, Math, LineNumerator, Optional, WavMaker, Version ;

const MAINHELP = 'Converter from Basic for BK-0010 to ASC-file for GID emulator'#13#10+
  'Version: '+TGitVersion.TAG+#13#10+
  'Usage: Basic_filename Asc_filename [parameters]'#13#10+
  'Parameters:'#13#10+
  '/basiccodepage=utf8|win1251|koi8r|oem866 - convert Basic file from codepage'#13#10+
  '/autonumlines=true|false - set line numbers to non-numbered Basic source'#13#10+
  '/savepreparedsource=true|false - save cleared and autonumbered (if use) source to .tmp file'#13#10+
  '/makewav=true|false - convert Basic program to WAV file for load into BK-0010 computer'#13#10+
  '/define=name - set name for ''$IFDEF directive' ;


ASC_NAME_LENGTH = 6 ;
HEADER_SIZE = 4 ;
MAX_BLOCK_SIZE = 256 ;

procedure TMain.ExitWithError(const msg: string; code: Integer);
begin
  Writeln(msg) ;
  Halt(code) ;
end;

function TMain.GetDefineCommandFromLine(const line: string;
  var defname: string): TDefBlockCommand;
begin
  Result:=dbcNone ;
  if line.Trim().IndexOf('''$IFDEF')=0 then begin
    Result:=dbcDefine ;
    defname:=line.Replace('''$IFDEF','').Trim().ToUpper() ;
  end;
  if line.Trim().IndexOf('''$ELSE')=0 then Result:=dbcElse ;
  if line.Trim().IndexOf('''$ENDIF')=0 then Result:=dbcEnd ;
end;

procedure TMain.Run() ;
var script:TStringList ;
    s,pname,pvalue:string ;
    i,j,p,filecnt,blocksize:Integer ;
    destfile,binfile:string ;
    srcenc:TEncoding ;
    makewav:Boolean ;
    data:TList<Byte> ;
    stm:TFileStream ;
    buf:TBytes ;
    ln:TLineNumerator ;
    autonumlines,savepreparedsource:Boolean ;
    newlines:TOptional<TStringList> ;
    wm:TWavMaker ;
    wavname:string ;
    deflist:TStringList ;
    currentdefblock:string ;
    currentdefblockstate:TDefBlockState ;
begin
  try
    if ParamCount<1 then ExitWithError(MAINHELP,1) ;

    deflist:=TStringList.Create ;
    srcenc:=TEncoding.UTF8 ;
    autonumlines:=False ;
    savepreparedsource:=False ;
    makewav:=False ;
    for i := 3 to ParamCount do begin
      if ParamStr(i)[1]<>'/' then ExitWithError('Unknown argument: '+ParamStr(i)+', use /name=value',2) ;
      p:=ParamStr(i).IndexOf('=') ;
      if p=-1 then ExitWithError('Unknown argument: '+ParamStr(i)+', use /name=value',3) ;
      pname:=ParamStr(i).Substring(1,p-1) ;
      pvalue:=ParamStr(i).Substring(p+1).ToLower() ;
      if pname='basiccodepage' then begin
        if (pvalue<>'utf8') and (pvalue<>'win1251') and
           (pvalue<>'koi8r') and (pvalue<>'oem866')  then
          ExitWithError('Unknown basiccodepage: '+pvalue,4) ;
        if pvalue='win1251' then srcenc:=TEncoding.GetEncoding(1251) ;
        if pvalue='oem866' then srcenc:=TEncoding.GetEncoding(866) ;
        if pvalue='koi8r' then srcenc:=TEncoding.GetEncoding(20866) ;
      end
      else
      if pname='autonumlines' then autonumlines:=pvalue='true'
      else
      if pname='savepreparedsource' then savepreparedsource:=pvalue='true'
      else
      if pname='makewav' then makewav:=pvalue='true'
      else
      if pname='define' then deflist.Add(pvalue.ToUpper())
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
    script.LoadFromFile(ParamStr(1),srcenc) ;

    i:=0 ;
    currentdefblock:='' ;
    currentdefblockstate:=dbsNone ;
    while i<script.Count do begin
      case GetDefineCommandFromLine(script[i],currentdefblock) of
        dbcDefine:
          if currentdefblockstate=dbsNone then
            currentdefblockstate:=dbsThen
          else
            ExitWithError('Multilevel $IFDEF not supported yet',1) ;
        dbcElse:
          if currentdefblockstate=dbsThen then
            currentdefblockstate:=dbsElse
          else
            ExitWithError('$ELSE without $IFDEF',1) ;
        dbcEnd:
          if currentdefblockstate in [dbsThen,dbsElse] then
            currentdefblockstate:=dbsNone
          else
            ExitWithError('$ENDIF without $ELSE or $IDFEF',1) ;
      end;
      script[i]:=StripCommentFromLine(script[i]).Trim() ;
      if script[i].Length=0 then script.Delete(i) else
      if (currentdefblockstate=dbsThen)and
        (deflist.IndexOf(currentdefblock)=-1) then script.Delete(i) else
      if (currentdefblockstate=dbsElse)and
        (deflist.IndexOf(currentdefblock)<>-1) then script.Delete(i) else
         Inc(i) ;
    end;

    if autonumlines then begin
      ln:=TLineNumerator.Create(script) ;
      newlines:=ln.getNumeratedLines() ;
      if (newlines) then begin
        script.Free ;
        script:=newlines.Value ;
      end
      else
        ExitWithError('Error enumerate lines: '+ln.getErrMsg(),10) ;
      ln.Free ;
    end;

    if savepreparedsource then script.SaveToFile(ParamStr(1)+'.tmp',srcenc) ;

    data:=TList<Byte>.Create() ;
    for s in script do begin
      data.AddRange(TEncoding.GetEncoding(20866).GetBytes(s.Trim())) ;
      data.Add($0A) ;
    end;
    data.Add($1A) ;
    script.Free ;

    if makewav then
      Writeln('Writeln Wav file...')
    else
      Writeln('Writeln Asc files...') ;

    if not destfile.ToLower().EndsWith('.asc') then destfile:=destfile+'.asc' ;

    filecnt:=data.Count div MAX_BLOCK_SIZE ;
    if data.Count mod MAX_BLOCK_SIZE<>0 then Inc(filecnt) ;

    wm:=TWavMaker.Create() ;
    wavname:=ExtractFileName(destfile.ToUpper()) ;

    for i := 0 to filecnt-1 do begin
      blocksize:=IfThen(i=filecnt-1,data.Count-(filecnt-1)*MAX_BLOCK_SIZE,MAX_BLOCK_SIZE) ;
      SetLength(buf,blocksize+HEADER_SIZE) ;
      buf[0]:=$EE ;
      buf[1]:=$3D ;
      buf[2]:=Byte(blocksize mod MAX_BLOCK_SIZE) ;
      buf[3]:=Byte(blocksize div MAX_BLOCK_SIZE) ;
      for j := 0 to blocksize-1 do
        buf[HEADER_SIZE+j]:=data[i*MAX_BLOCK_SIZE+j] ;

      if makewav then
        wm.AppendBinData(buf,Format('%s #%.3d',[wavname,i])+Chr(26))
      else begin
        binfile:=Format('%s #%.3d.bin',[destfile,i]) ;
        if FileExists(binfile) then SysUtils.DeleteFile(binfile) ;

        stm:=TFileStream.Create(binfile,fmCreate) ;
        stm.WriteBuffer(buf[0],Length(buf)) ;
        stm.Free ;
      end;
    end;

    SetLength(buf,6) ;
    buf[0]:=$EE ;
    buf[1]:=$3D ;
    buf[2]:=Byte(2) ;
    buf[3]:=Byte(0) ;
    buf[4]:=Byte(0) ;
    buf[5]:=Byte(0) ;

    if makewav then begin
      wm.AppendBinData(buf,wavname+Chr(32)+Chr(32)+StringOfChar(Chr(0),4)) ;
      wm.WriteToWav(destfile+'.wav') ;
    end
    else begin
      binfile:=destfile+'.bin' ;
      if FileExists(binfile) then SysUtils.DeleteFile(binfile) ;

      stm:=TFileStream.Create(binfile,fmCreate) ;
      stm.WriteBuffer(buf[0],Length(buf)) ;
      stm.Free ;
    end;

    Writeln('File(s) written, use LOAD "'+
      wavname.Replace('.ASC','').Trim()+'" for loading BASIC program') ;

    data.Free ;
    deflist.Free ;

  except
    on E: Exception do
      Writeln('Error '+E.ClassName+': '+E.Message);
  end;
end ;

function TMain.StripCommentFromLine(const line: string): string;
var i:Integer ;
    instring:Boolean ;
begin
  instring:=False ;
  for i := 0 to line.Length-1 do begin
    if line[i]='"' then instring:=not instring ;
    if not instring then
      if (line[i]='''')or (line.Substring(i,3).ToUpper()='REM') then
        Exit(line.Substring(0,i-1)) ;
  end;
  Result:=line ;
end;

end.
