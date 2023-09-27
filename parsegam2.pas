unit ParseGam2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  Utils, UserTypes, TffObjects;

function Gam2Parser(): TFrameRecords;
procedure Gam2DataChannelsSet(Tff_Ver: Byte);
function ReadValidDateTime(var DateTime: TDateTime): Boolean;
function FindFirstValidTime(): Boolean; { Search for valid Date/Time index }

implementation
uses Main;

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
    if ValidDateTime And (y >= 20) And (y <= 30) then begin
       DateTime:= EncodeDateTime(2000 + y, m, d, h, n, s, 0);
       Result:= True;
    end
    else Result:= False;
end;

function FindFirstValidTime(): Boolean; { Search for valid Date/Time index }
var
  ValidDateTime    : Boolean;
  b                : Byte;
  DateTime         : TDateTime;
begin
  b:= ReadCurrentByte;
  while (b = $FF) And (Not EndOfFile) do b:= ReadCurrentByte;
  Dec(DataOffset);
  repeat
     ValidDateTime:= ReadValidDateTime(DateTime);
     if Not ValidDateTime then Dec(DataOffset, 5);
  until EndOfFile or ValidDateTime;
  if ValidDateTime And Not EndOfFile then begin
     Dec(DataOffset, 6);
     Result:= True;
  end
  else Result:= False;
end;

procedure Gam2DataChannelsSet(Tff_Ver: Byte);
begin
  TffStructure.Init;
  TffStructure.AddChannel('TIME', '100S', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('Satus', '', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('HV_PMT', 'V', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('U_Sup', 'V', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('Op_time', 'Sec', 'U4', '1', Tff_Ver);
  TffStructure.AddChannel('Blank', '', 'U1', '1', Tff_Ver);
end;

function Gam2Parser(): TFrameRecords;
var
  I1            : ShortInt;
  U1            : Byte;
  I2            : SmallInt;
  U2            : Word;
  I4            : LongInt;
  U4            : LongWord;
  U8            : QWord;
  F4            : Single;
  F8            : Double;
  DateTime      : TDateTime;
  TffFrames     : TTffFrames;

begin
  ErrorCode:= WRONG_FILE_FORMAT;
  TffFrames.Init;
  DataOffset:= 49; { Header 49 bytes }
  EndOfFile:= False;
  if FindFirstValidTime then begin
    ErrorCode:= NO_ERROR;
    repeat
       if ReadValidDateTime(DateTime) then begin
          TffFrames.AddRecord(DateTime, TffStructure.GetDataChannelSize, TffStructure.GetTFFDataChannels);

          Move(Bytes[DataOffset], U4, 4);
          if U4 <> $FFFFFFFF then begin
              Move(Bytes[DataOffset], U2, 2);
              TffFrames.AddData(TffStructure.GetOffsetByName('Satus'), U2);
              IncDataOffset(2);

              Move(Bytes[DataOffset], U2, 2);
              TffFrames.AddData(TffStructure.GetOffsetByName('HV_PMT'), U2);
              IncDataOffset(2);

              Move(Bytes[DataOffset], U2, 2);
              F4:= U2 / 2.2;
              TffFrames.AddData(TffStructure.GetOffsetByName('U_Sup'), F4);
              IncDataOffset(2);

              Move(Bytes[DataOffset], U4, 4);
              TffFrames.AddData(TffStructure.GetOffsetByName('Op_time'), U4);
              IncDataOffset(4);

              Move(Bytes[DataOffset], U1, 1);
              TffFrames.AddData(TffStructure.GetOffsetByName('Blank'), U1); { Dummy param for ADV 2 }
          end
          else IncDataOffset(10);
       end
    until EndOfFile Or (ErrorCode > 0);
  end;
  if TffFrames.GetFrameRecords = Nil then ErrorCode:= WRONG_FILE_FORMAT;
  Result:= TffFrames.GetFrameRecords;
  TffFrames.Done;
end;

end.

