unit WavMaker;

interface
uses Classes, SysUtils ;

type
  TWavMaker = class
  private
    stm:TMemoryStream ;
    procedure WriteByteAsWav(b:Byte) ;
    procedure WriteByteArray(arr:array of Byte) ;
    procedure WriteCheckSum(const data:TBytes) ;
  public
    constructor Create ;
    procedure AppendBinData(const data:TBytes; const binname:AnsiString) ;
    procedure WriteToWav(const filename:string) ;
    destructor Destroy ; override ;
  end;

implementation

const SUBCHUNKSIZE = 16 ;
      PCM = 1 ;
      MONO = 1 ;
      FREQ = 22050 ;
      BLOCKALIGN = 1 ;
      BITS = 8 ;

var ST_0:array[0..11] of Byte = (128,191,191,128,64,64,128,191,191,128,64,64) ;
var ST_1:array[0..17] of Byte = (128,160,183,192,183,160,128,95,72,63,72,95,128,183,183,128,72,72) ;
var ST_2:array[0..47] of Byte = (128,191,191,128,64,64,128,191,191,128,64,64,128,191,191,128,64,64,128,191,191,128,64,64,128,191,191,128,64,64,128,191,191,128,64,64,128,191,191,128,64,64,128,191,191,128,64,64) ;
var ST_SYNCHRO:array[0..41] of Byte = (128,144,157,164,166,169,169,170,170,170,170,170,128,111,98,91,87,86,85,85,85,85,85,110,128,154,162,166,167,169,128,111,99,92,89,89,128,183,183,128,72,72) ;
var ST_4:array[0..63] of Byte = (128,143,168,181,188,191,193,193,194,194,194,194,194,193,193,193,193,193,192,192,192,192,192,191,191,190,190,190,189,188,178,128,89,72,64,59,57,56,55,55,55,55,55,56,56,56,56,57,57,57,57,57,58,58,58,58,58,59,59,59,59,60,62,122) ;

{ TWavMaker }

procedure TWavMaker.AppendBinData(const data:TBytes; const binname:AnsiString);
var i:Integer ;
begin
  for i := 1 to 512 do
    WriteByteArray(ST_2) ;
  WriteByteArray(ST_SYNCHRO) ;

  WriteByteArray(ST_2) ;
  WriteByteArray(ST_SYNCHRO) ;

  for i := 0 to 3 do
    WriteByteAsWav(data[i]) ;

  for i := 1 to 16 do
    WriteByteAsWav(Ord(binname[i])) ;

  WriteByteArray(ST_2) ;
  WriteByteArray(ST_SYNCHRO) ;

  for i := 4 to Length(data)-1 do
    WriteByteAsWav(data[i]) ;

  WriteCheckSum(data) ;

  WriteByteArray(ST_4) ;

  for i := 1 to 32 do
    WriteByteArray(ST_2) ;
  WriteByteArray(ST_SYNCHRO) ;
end;

constructor TWavMaker.Create;
begin
  stm:=TMemoryStream.Create() ;
end;

destructor TWavMaker.Destroy;
begin
  stm.Free ;
  inherited Destroy;
end;

procedure TWavMaker.WriteByteArray(arr:array of Byte);
begin
  stm.WriteBuffer(arr[0],Length(arr)) ;
end;

procedure TWavMaker.WriteByteAsWav(b: Byte);
var i:Integer ;
begin
  for i := 0 to 7 do
    if (b and (1 shl i)) = 0 then
      WriteByteArray(ST_0)
    else
      WriteByteArray(ST_1) ;
end;

procedure TWavMaker.WriteCheckSum(const data: TBytes);
var i,sum:Integer ;
begin
  sum:=0 ;
  for i := 4 to Length(data)-1 do begin
    Inc(sum,data[i]) ;
    if sum>65535 then Dec(sum,65536) ;
  end;

  WriteByteAsWav(sum mod 256) ;
  WriteByteAsWav(sum div 256) ;
end;

procedure TWavMaker.WriteToWav(const filename: string);
var stmf:TFileStream ;

procedure WriteChars(const v:AnsiString) ;
begin
  stmf.WriteBuffer(v[1],Length(v)) ;
end;

procedure WriteUInt16(v:UInt16) ;
begin
  stmf.WriteBuffer(v,SizeOf(v)) ;
end;

procedure WriteUInt32(v:UInt32) ;
begin
  stmf.WriteBuffer(v,SizeOf(v)) ;
end;

begin
  stmf:=TFileStream.Create(filename,fmCreate) ;

  WriteChars('RIFF') ;
  WriteUInt32(stm.Size+36) ;
  WriteChars('WAVEfmt ') ;
  WriteUInt32(SUBCHUNKSIZE) ;
  WriteUInt16(PCM) ;
  WriteUInt16(MONO) ;
  WriteUInt32(FREQ) ;
  WriteUInt32(FREQ) ;
  WriteUInt16(BLOCKALIGN) ;
  WriteUInt16(BITS) ;
  WriteChars('data') ;
  WriteUInt32(stm.Size) ;

  stm.Position:=0 ;
  stmf.CopyFrom(stm,stm.Size) ;
  stmf.Free ;
end;

initialization

end.
