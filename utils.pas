unit Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  UserTypes, StrUtils;

function LoadByteArray(const AFileName: string): TBytes;
procedure SaveByteArray(AByteArray: TBytes; const AFileName: string);
function InArray(Value: Byte; Arr: TBytes): Boolean;
function FillSingle(H1, H0, L1, L0: Byte): Single;
function FillDouble(H3, H2, H1, H0, L3, L2, L1, L0: Byte): Double;
function FillInteger(H, L: Byte): SmallInt;
function FillLongInt(H1, H0, L1, L0, size: Byte): longInt;
function FillWord(H, L: Byte): Word;
function FillLongWord(H1, H0, L1, L0, size: Byte): longWord;
function FillInt64(H3, H2, H1, H0, L3, L2, L1, L0, size: Byte): Int64;
function FillQWord(H3, H2, H1, H0, L3, L2, L1, L0, size: Byte): QWord;
function GetTypeLegth(wType: String): Byte;
function FormatSeconds(i: Integer): String;
function LoadSourceFile(FileExt: String; MinFileSize: LongWord): Boolean;
function ReadCurrentByte(): Byte;
function isEndOfFile(): Boolean;
function SetStringLength(Str: String; n: Word): String;
procedure IncDataOffset(n: LongWord);
function GetErrorMessage(error: Byte): PChar;
function ReadValidDateTime(var DateTime: TDateTime): Boolean;

implementation
Uses Main;

function GetErrorMessage(error: Byte): PChar;
begin
  case error of
     0: Result:= 'NO_ERROR';
     1: Result:= 'FILE_NOT_FOUND';
     2: Result:= 'WRONG_FILE_FORMAT';
     3: Result:= 'UNEXPECTED_END_OF_FILE';
  end;
end;

function LoadByteArray(const AFileName: string): TBytes;
var
  AStream: TStream;
  ADataLeft: Integer;
begin
  SetLength(result, 0);
  AStream:= TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    try
      AStream.Position:= 0;
      ADataLeft:= AStream.Size;
      SetLength(Result, ADataLeft div SizeOf(Byte));
      AStream.Read(PByte(Result)^, ADataLeft);
    except
      on Exception : EStreamError do
         Result:= Null;
    end;
  finally
    AStream.Free;
  end;
end;

procedure SaveByteArray(AByteArray: TBytes; const AFileName: string);
var
  AStream: TStream;
begin
  if FileExists(AFileName) then DeleteFile(AFileName);
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
     AStream.WriteBuffer(Pointer(AByteArray)^, Length(AByteArray));
     App.Indicator.Caption:= 'File converted';
     App.Indicator.Refresh;
  finally
     AStream.Free;
  end;
end;

function LoadSourceFile(FileExt: String; MinFileSize: LongWord): Boolean; // Load bin file to the Bytes array
begin
  Result:= False;
  App.OpenDialog.Filter:= 'bin files|*.' + FileExt + '|all files|*.*|';
  App.OpenDialog.DefaultExt:= '.' + FileExt;
  if App.OpenDialog.Execute then begin
     Bytes:= LoadByteArray(App.OpenDialog.FileName);
     if Bytes <> Null then begin
        currentFileSize:= length(Bytes);
        DataOffset:= 0;
        if currentFileSize >= MinFileSize then begin
           EndOfFile:= False;
           Result:= True;
           CurrentOpenedFile:= App.OpenDialog.FileName;
           App.Indicator.Caption:= 'In progress';
           App.Indicator.Refresh;
        end;
     end;
  end;
end;

function ReadCurrentByte(): Byte;
begin
  if Not EndOfFile then begin
     Result:= Bytes[DataOffset];
     Inc(DataOffset);
     if DataOffset >= currentFileSize then EndOfFile:= True;
  end;
end;

procedure IncDataOffset(n: LongWord);
var i: LongWord;
begin
   for i:=1 to n do ReadCurrentByte;
end;

function ReadValidDateTime(var DateTime: TDateTime): Boolean;
var Flag             : Boolean;
    y, m, d, h, n, s : longInt;
    ValidDateTime    : Boolean;
begin
   try
      Flag:= TryStrToInt(IntToHex(ReadCurrentByte), y);
      Flag:= TryStrToInt(IntToHex(ReadCurrentByte), m);
      Flag:= TryStrToInt(IntToHex(ReadCurrentByte), d);
      Flag:= TryStrToInt(IntToHex(ReadCurrentByte), h);
      Flag:= TryStrToInt(IntToHex(ReadCurrentByte), n);
      Flag:= TryStrToInt(IntToHex(ReadCurrentByte), s);
      if flag then ValidDateTime:= IsValidDateTime(2000 + y, m, d, h, n, s, 0)
      else ValidDateTime:= False;
    finally
    end;
    if ValidDateTime And (y >= 22) And (y <= 30) then begin
       DateTime:= EncodeDateTime(2000 + y, m, d, h, n, s, 0);
       Result:= True;
    end
    else Result:= False;
end;

function isEndOfFile(): Boolean;
begin
  if DataOffset >= currentFileSize then begin
     EndOfFile:= True;
     Result:= True;
  end
  else Result:= False;
end;

function GetTypeLegth(wType: String): Byte;
begin
  Result:= StrToInt(wType[2]);
end;

function SetStringLength(Str: String; n: Word): String;
begin
  Result:= AddCharR(' ', LeftBStr(Str, n), n);
end;

function InArray(Value: Byte; Arr: TBytes): Boolean;
var i, Len: Word;
begin
  Len:= Length(Arr);
  for i:=0 to Len - 1 do
    if Value = Arr[i] then begin
       Result:= True;
       Exit;
    end;
  Result:= False;
end;

function FillSingle(H1, H0, L1, L0: Byte): Single;
var Flt: Single;
    FltBytes: array[1..3] of Byte absolute Flt;
begin
   Flt:= 0;
   FltBytes[1]:= L0;
   FltBytes[2]:= L1;
   FltBytes[3]:= H0;
   FltBytes[4]:= H1;
   Result:= Flt;
end;

function FillDouble(H3, H2, H1, H0, L3, L2, L1, L0: Byte): Double;
var Flt: Double;
    FltBytes: array[1..8] of Byte absolute Flt;
begin
   Flt:= 0;
   FltBytes[1]:= L0;
   FltBytes[2]:= L1;
   FltBytes[3]:= L2;
   FltBytes[4]:= L3;
   FltBytes[5]:= H0;
   FltBytes[6]:= H1;
   FltBytes[7]:= H2;
   FltBytes[8]:= H3;
   Result:= Flt;
end;

function FillInteger(H, L: Byte): SmallInt;
var lW: SmallInt;
    lWBytes: array[1..2] of ShortInt absolute lW;
begin
   lW:= 0;
   lWBytes[1]:= L;
   lWBytes[2]:= H;
   Result:= lW;
end;

function FillLongInt(H1, H0, L1, L0, size: Byte): longInt;
var lW: longInt;
    lWBytes: array[1..4] of ShortInt absolute lW;
begin
   lW:= 0;
   lWBytes[1]:= L0;
   lWBytes[2]:= L1;
   lWBytes[3]:= H0;
   if size = 4 then lWBytes[4]:= H1
   else lWBytes[4]:= 0;
   Result:= lW;
end;

function FillWord(H, L: Byte): Word;
var lW: Word;
    lWBytes: array[1..2] of Byte absolute lW;
begin
   lW:= 0;
   lWBytes[1]:= L;
   lWBytes[2]:= H;
   Result:= lW;
end;

function FillLongWord(H1, H0, L1, L0, size: Byte): longWord;
var lW: longWord;
    lWBytes: array[1..4] of Byte absolute lW;
begin
   lW:= 0;
   lWBytes[1]:= L0;
   lWBytes[2]:= L1;
   lWBytes[3]:= H0;
   lWBytes[4]:= H1;
   if size = 4 then lWBytes[4]:= H1
   else lWBytes[4]:= 0;
   Result:= lW;
end;

function FillInt64(H3, H2, H1, H0, L3, L2, L1, L0, size: Byte): Int64;
var lW: Int64;
    lWBytes: array[1..8] of ShortInt absolute lW;
    i: Byte;
begin
   lW:= 0;
   lWBytes[1]:= L0;
   lWBytes[2]:= L1;
   lWBytes[3]:= L2;
   lWBytes[4]:= L3;
   lWBytes[5]:= H0;
   lWBytes[6]:= H1;
   lWBytes[7]:= H2;
   lWBytes[8]:= H3;
   for i:=8 downto size + 1 do lWBytes[i]:= 0;
   Result:= lW;
end;

function FillQWord(H3, H2, H1, H0, L3, L2, L1, L0, size: Byte): QWord;
var lW: QWord;
    lWBytes: array[1..8] of Byte absolute lW;
    i: Byte;
begin
   lW:= 0;
   lWBytes[1]:= L0;
   lWBytes[2]:= L1;
   lWBytes[3]:= L2;
   lWBytes[4]:= L3;
   lWBytes[5]:= H0;
   lWBytes[6]:= H1;
   lWBytes[7]:= H2;
   lWBytes[8]:= H3;
   for i:=8 downto size + 1 do lWBytes[i]:= 0;
   Result:= lW;
end;

function FormatSeconds(i: Integer): String;
begin
   FormatSeconds:= IntToStr(i);
end;

end.


