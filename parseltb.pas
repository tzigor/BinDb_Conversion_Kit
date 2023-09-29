unit ParseLTB;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  Utils, UserTypes, TffObjects;

procedure LTBDataChannelsSet(Tff_Ver: Byte);
function LTBParser(): TFrameRecords;

implementation
uses Main;

procedure LTBDataChannelsSet(Tff_Ver: Byte);
begin
  TffStructure.Init;
  TffStructure.AddChannel('TIME', '100S', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('Temp', 'C', 'I2', '1', Tff_Ver);
  TffStructure.AddChannel('Status', '', 'U4', '1', Tff_Ver);
  TffStructure.AddChannel('U_LTB', 'V', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('I_LTB', 'A', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('Shock', 'Cnt', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('G_LTB', 'g', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('LTB_Errors', 'Cnt', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('Blank', '', 'U1', '1', Tff_Ver);
end;

function LTBParser(): TFrameRecords;
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
  DataOffset:= 64; { Header 64 bytes }
  EndOfFile:= False;
  ErrorCode:= NO_ERROR;
  repeat
     IncDataOffset(2);
     if ReadValidDateTime(DateTime) then begin
        TffFrames.AddRecord(DateTime, TffStructure.GetDataChannelSize, TffStructure.GetTFFDataChannels);

        Move(Bytes[DataOffset], U4, 4);
        TffFrames.AddData(TffStructure.GetOffsetByName('Status'), U4);
        IncDataOffset(4);

        Move(Bytes[DataOffset], U2, 2);
        F4:= U2 / 10;
        TffFrames.AddData(TffStructure.GetOffsetByName('U_LTB'), F4);
        IncDataOffset(2);

        Move(Bytes[DataOffset], U2, 2);
        F4:= U2 / 1000;
        TffFrames.AddData(TffStructure.GetOffsetByName('I_LTB'), F4);
        IncDataOffset(2);

        Move(Bytes[DataOffset], I2, 2);
        TffFrames.AddData(TffStructure.GetOffsetByName('Temp'), I2);
        IncDataOffset(2);

        Move(Bytes[DataOffset], U2, 2);
        TffFrames.AddData(TffStructure.GetOffsetByName('Shock'), U2);
        IncDataOffset(2);

        Move(Bytes[DataOffset], U2, 2);
        TffFrames.AddData(TffStructure.GetOffsetByName('G_LTB'), U2);
        IncDataOffset(2);

        Move(Bytes[DataOffset], U2, 2);
        TffFrames.AddData(TffStructure.GetOffsetByName('LTB_Errors'), U2);
        IncDataOffset(2);

        IncDataOffset(8);

        TffFrames.AddData(TffStructure.GetOffsetByName('Blank'), 0); { Dummy param for ADV 2 }
     end
  until EndOfFile Or (ErrorCode > 0);
  if TffFrames.GetFrameRecords = Nil then ErrorCode:= WRONG_FILE_FORMAT;
  Result:= TffFrames.GetFrameRecords;
  TffFrames.Done;
end;

end.

