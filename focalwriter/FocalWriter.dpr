program FocalWriter;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Classes, Generics.Collections;

type
  TFocalLine = record
    num1:Integer ;
    num2:Integer ;
    command:string ;
  end;

// Замены выражений в Фокале
const COMMANDS_ALIAS:array [0..11,0..1] of string = (
   ('ASK','A'),
   ('COMMENT','C'),
   ('DO','D'),
   ('ERASE','E'),
   ('SET','S'),
   ('XECUTE','X'),
   ('FOR','F'),
   ('IF','I'),
   ('GOTO','G'),
   ('QUIT','Q'),
   ('RETURN','R'),
   ('TYPE','T')
   ) ;

function getFocalFloatAsByte(ff:Integer):Byte ;
begin
  Result:=Trunc(256*((ff)/100)) ;
end;

function CreateFocalProg(const prog:TStringList):TList<TFocalLine> ;
var tmp:TArray<string> ;
    fl:TFocalLine ;
    i:Integer ;
    s:string ;
begin
  Result:=TList<TFocalLine>.Create ;
  for s in prog do begin
    if Length(Trim(s))=0 then Continue ;
    if Trim(s)[1]='#' then Continue ;
    if Trim(s).Substring(0,2)='//' then Continue ;

    tmp:=Trim(s).Split([' '],TStringSplitOptions.ExcludeEmpty) ;
    fl.command:=Trim(Trim(s).Substring(tmp[0].Length)) ;
    tmp:=tmp[0].Split(['.']) ;
    fl.num1:=StrToInt(tmp[0]) ;
    if tmp[1].Length=1 then tmp[1]:=tmp[1]+'0' ;
    fl.num2:=StrToInt(tmp[1]) ;

    for i := 0 to Length(COMMANDS_ALIAS)-1 do begin
      if fl.command.StartsWith(COMMANDS_ALIAS[i][0]+' ') then begin
        fl.command:=COMMANDS_ALIAS[i][1]+' '+fl.command.Substring(Length(COMMANDS_ALIAS[i][0])+1) ;
        Break ;
      end;
      if fl.command=COMMANDS_ALIAS[i][0] then begin
        fl.command:=COMMANDS_ALIAS[i][1] ;
        Break ;
      end;
    end;

    Result.Add(fl) ;
  end;
end;

procedure DoWriteFocal(const lines:TList<TFocalLine>; const binfile:string) ;
var stm:TFileStream ;
    head,buf,bufnum:TBytes ;
    i,j,k,cnt,size:Integer ;
    start:Cardinal ;
    fl:TFocalLine ;

const TERM = $8E ; // Конец строки Фокала
      PARITYADD = $00 ; // Добавление в конце строки для четности

// Замены символов в Фокале
const REPLACEMENTS:array [0..12,0..1] of Byte = (
   (ord(' '),$80),
   (ord('+'),$81),
   (ord('-'),$82),
   (ord('/'),$83),
   (ord('*'),$84),
   (ord('^'),$85),
   (ord('('),$86),
   (ord('['),$87),
   (ord(')'),$89),
   (ord(']'),$8A),
   (ord(','),$8C),
   (ord(';'),$8D),
   (ord('='),$8F)
   ) ;

begin

  // Расчет размера для заголовка
  size:=22 ;
  for fl in lines do begin
    cnt:=TEncoding.ANSI.GetByteCount(fl.command) ;
    Inc(size,cnt+5) ;
    // Коррекция на четность
    if cnt mod 2 = 0 then Inc(size) ;
  end;

  // Заголовок с константами и размером
  SetLength(head,26) ;
  head[0]:=$EA ;
  head[1]:=$03 ;
  head[2]:=Byte(size mod 256) ;
  head[3]:=Byte(size div 256) ;
  head[4]:=$14 ; head[5]:=$00 ; head[6]:=$00 ; head[7]:=$00 ;

  head[8]:=ord('C') ; head[9]:=ord(':') ;  head[10]:=$20 ; head[11]:=$20 ;
  head[12]:=$E6 ; head[13]:=$EF ; head[14]:=$EB ; head[15]:=$E1 ;
  head[16]:=$EC ; head[17]:=$2D ; head[18]:=$E2 ; head[19]:=$EB ;
  head[20]:=$30 ; head[21]:=$30 ; head[22]:=$31 ; head[23]:=$30 ;
  head[24]:=TERM ; head[25]:=PARITYADD ;

  SetLength(bufnum,4) ;
  start:=$FBFE ; // Магическое число размера файла для контрольной суммы

  stm:=TFileStream.Create(binfile,fmCreate) ;
  stm.WriteBuffer(head,Length(head)) ;

  for i:=0 to lines.count-1 do begin
    buf:=TEncoding.ANSI.GetBytes(lines[i].command) ;
    // Замена символов на фокальные
    for j := 0 to Length(buf)-1 do
      for k := 0 to Length(REPLACEMENTS)-1 do
        if buf[j]=REPLACEMENTS[k,0] then buf[j]:=REPLACEMENTS[k,1] ;

    // Добивка до четности
    if Length(buf) mod 2 = 0 then begin
      SetLength(buf,Length(buf)+2) ;
      buf[Length(buf)-2]:=TERM ;
      buf[Length(buf)-1]:=PARITYADD ;
    end
    else begin
      SetLength(buf,Length(buf)+1) ;
      buf[Length(buf)-1]:=TERM ;
    end;

    // Либо контрольная сумма для последней строки, либо длина строки
    if i=lines.count-1 then begin
      bufnum[0]:=Byte(start mod 256);
      bufnum[1]:=Byte(start div 256);
    end
    else begin
      bufnum[0]:=Length(buf)+2 ;
      bufnum[1]:=$00 ;
    end;
    bufnum[2]:=getFocalFloatAsByte(lines[i].num2) ;
    bufnum[3]:=Byte(lines[i].num1) ;

    // Обновление контрольной суммы
    Dec(start,Length(buf)+4) ;

    stm.WriteBuffer(bufnum,Length(bufnum)) ;
    stm.WriteBuffer(buf,Length(buf)) ;
  end;

  stm.Free ;
end;

// Задачи: запрет на одинаковые строки и на строку с большим номером перед меньшим

var prog:TStringList ;
    lines:TList<TFocalLine> ;
begin
  try
    if ParamCount<2 then begin
      Writeln('Converter from Focal for BK-0010 to BIN-file for GID emulator') ;
      Writeln('Usage: Focal_filename BIN_filename') ;
      Halt(1) ;
    end ;
    prog:=TStringList.Create ;
    prog.LoadFromFile(ParamStr(1)) ;
    lines:=CreateFocalProg(prog) ;
    DoWriteFocal(lines,ParamStr(2)) ;
    lines.Free ;
    prog.Free ;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
