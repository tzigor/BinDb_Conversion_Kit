unit ParseGam;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  Utils, UserTypes, TffObjects, ParseGam2;

procedure GamDataChannelsSet(Tff_Ver: Byte);
function GamParser(): TFrameRecords;

implementation
uses Main;

procedure GamDataChannelsSet(Tff_Ver: Byte);
begin
  TffStructure.Init;
  TffStructure.AddChannel('TIME', '100S', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('Temp', 'C', 'I1', '1', Tff_Ver);
  TffStructure.AddChannel('ngrth1b', 'Cnt', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('Status', '', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Blank', '', 'U1', '1', Tff_Ver);
end;

function GamParser(): TFrameRecords;
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
  TimeScan      : Byte;
  i, Parameter  : Byte;
  Counter       : Byte;

begin
  ErrorCode:= WRONG_FILE_FORMAT;
  ErrorCode:= 0;
  TffFrames.Init;
  DataOffset:= 44 + 32; { Header 44 bytes + 2 records (?) }
  EndOfFile:= False;
  Counter:= 0;
  if FindFirstValidTime then begin
    ErrorCode:= NO_ERROR;
    TimeScan:= 1;
    repeat
       if ReadValidDateTime(DateTime) then begin
          TffFrames.AddRecord(DateTime, TffStructure.GetDataChannelSize, TffStructure.GetTFFDataChannels);

          Parameter:= ReadCurrentByte;
          if Parameter = $FF then Counter:= 0;

          //if Counter = 1 then TimeScan:= Parameter;
          if Counter = 3 then TffFrames.AddData(TffStructure.GetOffsetByName('Status'), Parameter);
          Inc(Counter);

          Move(Bytes[DataOffset], I1, 1);
          TffFrames.AddData(TffStructure.GetOffsetByName('Temp'), I1);
          IncDataOffset(1);

          Move(Bytes[DataOffset], U2, 2);
          TffFrames.AddData(TffStructure.GetOffsetByName('ngrth1b'), U2);
          IncDataOffset(2);

          for i:=1 to 3 do begin
             DateTime:= IncSecond(DateTime, TimeScan);
             TffFrames.AddRecord(DateTime, TffStructure.GetDataChannelSize, TffStructure.GetTFFDataChannels);
             TffFrames.AddData(TffStructure.GetOffsetByName('Temp'), I1);

             Move(Bytes[DataOffset], U2, 2);
             TffFrames.AddData(TffStructure.GetOffsetByName('ngrth1b'), U2);
             IncDataOffset(2);
          end;

          //TffFrames.AddData(TffStructure.GetOffsetByName('Blank'), 255); { Dummy param for ADV 2 }

       end
    until EndOfFile Or (ErrorCode > 0);
  end;
  if TffFrames.GetFrameRecords = Nil then ErrorCode:= WRONG_FILE_FORMAT;
  Result:= TffFrames.GetFrameRecords;
  TffFrames.Done;
end;

end.

