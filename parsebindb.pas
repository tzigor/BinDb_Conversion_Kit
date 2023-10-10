unit ParseBinDb;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  Utils, UserTypes, TffObjects, StrUtils;

function BinDbParser(Tff_Ver: Byte): TFrameRecords;

implementation
uses Main;

var
    StartDate     : TDateTime;
    StartTime     : TDateTime;
    FullDateTime  : TDateTime;
    TffFrames     : TTffFrames;
    isEnergiya    : Boolean;

function FindStartDate(Parameter: String; var DateTime: TDateTime): Boolean;
begin
   Result:= False;
   if (NPos('START', UpperCase(Parameter), 1) > 0) And
            (NPos('DATE', UpperCase(Parameter), 1) > 0) then begin
      Copy2SymbDel(Parameter, '=');
      try
        DateTime:= ScanDateTime('dd-mmm-yyyy', DelSpace(Parameter));
        Result:= True;
      except
        on Exception : EConvertError do Result:= False;
      end;
   end;
end;

function FindStartTime(Parameter: String; var DateTime: TDateTime): Boolean;
begin
   Result:= False;
   if (NPos('START', UpperCase(Parameter), 1) > 0) And
            (NPos('TIME', UpperCase(Parameter), 1) > 0) then begin
      Copy2SymbDel(Parameter, '=');
      try
        DateTime:= ScanDateTime('hh:mm:ss', DelSpace(Parameter));
        Result:= True;
      except
        on Exception : EConvertError do Result:= False;
      end;
   end;
end;

Function GetRecordLength(): LongWord;
var RecordLen    : LongWord;
    RecordType   : Char;
begin
  RecordLen:= ReadCurrentByte;
  RecordLen:= (RecordLen or (ReadCurrentByte << 8)) - 1;
  RecordType:= Chr(ReadCurrentByte);
  if RecordType = 'L' then begin
      RecordLen:= RecordLen or (ReadCurrentByte << 16);
      RecordLen:= (RecordLen or (ReadCurrentByte << 24)) - 1;
  end;
  Dec(DataOffset);
  Result:= RecordLen;
end;

function DataToStr(RecordLength: longWord): String;
var len, i : Word;
    wStr   : String;
    b      : Byte;
begin
  wStr:= '';
  for i:=1 to RecordLength do begin
     b:= ReadCurrentByte;
     if b > 0 then wStr:= wStr + Chr(b);
  end;
  Result:= wStr;
end;

function ParseDataChannel(RecordLength: longWord): TTFFDataChannel;
var DataChannel    : TTFFDataChannel;
    DLISNameLen,
    UnitsLen,
    RepCodeLen,
    SamplesLen,
    AbsentValueLen : Byte;
    i, b           : Byte;
    wSamples       : QWord;
begin
  DLISNameLen:= 10;
  UnitsLen:= 4;
  RepCodeLen:= 2;
  SamplesLen:= 10;
  if RecordLength < 42 then SamplesLen:= 4;
  if RecordLength >= 52 then DLISNameLen:= 16;
  AbsentValueLen:= RecordLength - DLISNameLen - UnitsLen - RepCodeLen - SamplesLen;
  with DataChannel do begin
     DLIS:= '';
     Units:= '';
     RepCode:= '';
     Samples:= '';
     AbsentValue:= '';
     for i:= 0 to DLISNameLen - 1 do begin
        b:= ReadCurrentByte;
        if b > 0 then DLIS:= DLIS + Chr(b);
     end;
     for i:= 0 to UnitsLen - 1 do begin
        b:= ReadCurrentByte;
        if b > 0 then Units:= Units + Chr(b);
     end;
     for i:= 0 to RepCodeLen - 1 do begin
        b:= ReadCurrentByte;
        if b > 0 then RepCode:= RepCode + Chr(b);
     end;
     for i:= 0 to SamplesLen - 1 do begin
        b:= ReadCurrentByte;
        if b > 0 then Samples:= Samples + Chr(b);
     end;
     wSamples:= StrToInt(Samples);
     if wSamples > 1 then begin
        DLIS:= DLIS + '[]';
        isEnergiya:= True;
        App.EnergiyaType.Visible:= True;
     end;
     for i:= 0 to AbsentValueLen - 1 do begin
        b:= ReadCurrentByte;
        if b > 0 then AbsentValue:= AbsentValue + Chr(b);
     end;
  end;
  Result:= DataChannel;
end;

procedure ParseFrame(RecordLength: longWord);
var
   F4        : Single;
   TimeShift : Single;
begin
   FullDateTime:= StartDate + StartTime;
   TimeShift:= SecondsBetween(FullDateTime, StartDate) / 100;
   Move(Bytes[DataOffset], F4, 4);
   Inc(DataOffset, 4);
   FullDateTime:= IncSecond(FullDateTime, Round((F4 - TimeShift) * 100));
   TffFrames.AddRecord(FullDateTime, TffStructure.GetDataChannelSize, TffStructure.GetTFFDataChannels);
   TffFrames.MoveData();
   Inc(DataOffset, RecordLength - 4);
end;

procedure ParseFrameNRG(ChannelsList: TTFFDataChannels);
var i, ChannelsCount : Word;
    I1               : ShortInt;
    U1               : Byte;
    I2               : SmallInt;
    U2               : Word;
    I4               : LongInt;
    U4               : LongWord;
    U8               : QWord;
    F4               : Single;
    F8               : Double;
    wSamples         : LongInt;
    TimeShift        : Single;

begin
   FullDateTime:= StartDate + StartTime;
   TimeShift:= SecondsBetween(FullDateTime, StartDate) / 100;
   ChannelsCount:= length(ChannelsList);
   for i:=0 to ChannelsCount - 1 do begin
      TryStrToInt(ChannelsList[i].Samples, wSamples);
      case ChannelsList[i].RepCode of
        'F4': begin
                Move(Bytes[DataOffset], F4, 4);
                if i = 0 then begin  { if TIME channel }
                   FullDateTime:= IncSecond(FullDateTime, Round((F4 - TimeShift) * 100));
                   TffFrames.AddRecord(FullDateTime, TffStructure.GetDataChannelSize, TffStructure.GetTFFDataChannels);
                   IncDataOffset(4);
                end
                else begin
                   TffFrames.AddData(ChannelsList[i].Offset, F4);
                   if (F4 = -999.25) or (wSamples = 1) then IncDataOffset(4)
                   else IncDataOffset(4 * wSamples);
                end;
              end;
        'F8': begin
                Move(Bytes[DataOffset], F8, 8);
                TffFrames.AddData(ChannelsList[i].Offset, F8);
                if (F8 = -999.25) or (wSamples = 1) then IncDataOffset(8)
                else IncDataOffset(8 * wSamples);
              end;
        'I1': begin
                I1:= ReadCurrentByte;
                TffFrames.AddData(ChannelsList[i].Offset, I1);
                if (I1 <> 127) And (wSamples > 1) then IncDataOffset(wSamples - 1);
              end;
        'U1': begin
                U1:= ReadCurrentByte;
                TffFrames.AddData(ChannelsList[i].Offset, U1);
                if (U1 <> 255) And (wSamples > 1) then IncDataOffset(wSamples - 1);
              end;
        'I2': begin
                 Move(Bytes[DataOffset], I2, 2);
                 TffFrames.AddData(ChannelsList[i].Offset, I2);
                 if I2 = 32767 then IncDataOffset(2)
                 else IncDataOffset(2 * wSamples);
              end;
        'U2': begin
                 Move(Bytes[DataOffset], U2, 2);
                 TffFrames.AddData(ChannelsList[i].Offset, U2);
                 if (U2 = 65535) or (wSamples = 1) then IncDataOffset(2)
                 else IncDataOffset(2 * wSamples);
              end;
        'U4': begin
                 Move(Bytes[DataOffset], U4, 4);
                 TffFrames.AddData(ChannelsList[i].Offset, U4);
                 if (U4 = 4294967295) or (wSamples = 1) then IncDataOffset(4)
                 else IncDataOffset(4 * wSamples);
              end;
        'I4': begin
                 Move(Bytes[DataOffset], I4, 4);
                 TffFrames.AddData(ChannelsList[i].Offset, I4);
                 if (I4 = 2147483647) or (wSamples = 1) then IncDataOffset(4)
                 else IncDataOffset(4 * wSamples);
              end;
        'U8': begin
                 Move(Bytes[DataOffset], U8, 8);
                 TffFrames.AddData(ChannelsList[i].Offset, U8);
                 IncDataOffset(8);
              end
      end;
   end;
end;

function BinDbParser(Tff_Ver: Byte): TFrameRecords;
var
    RecordType      : Char;
    NewParameter    : String;

    ParamChannels   : TStringList;
    RecordLength    : LongWord;
    DataChannel     : TTFFDataChannel;
    DateTime        : TDateTime;
    i, iPrevPercent : Integer;

    wStartTime, wEndTime : TDateTime;

begin

  wStartTime:= Time;

  ErrorCode:= NO_ERROR;
  isEnergiya:= False;
  TffStructure.Init;
  TffFrames.Init;
  ProgressInit(CurrentFileSize, 'Parsing');
  iPrevPercent:= 0;
  ParamChannels:= TStringList.Create;
  repeat
     RecordLength:= GetRecordLength;
     RecordType:= Chr(ReadCurrentByte);
     if Not EndOfFile then begin
         case RecordType of
            'P': begin
                    NewParameter:= DataToStr(RecordLength);
                    ParamChannels.Add(NewParameter);
                    if FindStartDate(NewParameter, DateTime) then begin
                       StartDate:= DateTime;
                       ParamChannels.Delete(ParamChannels.Count - 1);
                    end;
                    if FindStartTime(NewParameter, DateTime) then begin
                       StartTime:= DateTime;
                       ParamChannels.Delete(ParamChannels.Count - 1);
                    end;
                 end;
            'M': IncDataOffset(RecordLength);
            'D': begin
                    DataChannel:= ParseDataChannel(RecordLength);
                    TffStructure.AddChannel(DataChannel.DLIS, DataChannel.Units, DataChannel.RepCode, DataChannel.Samples, Tff_Ver);
                 end;
            'F': begin
                    if isEnergiya then ParseFrameNRG(TffStructure.GetTFFDataChannels)
                    else ParseFrame(RecordLength);
                 end;
            'B': IncDataOffset(RecordLength);
         else ErrorCode:= WRONG_FILE_FORMAT;
         end;
     end;

     i:= Trunc(DataOffset * 100 / CurrentFileSize);
     if (i > iPrevPercent) then begin
       App.ProcessProgress.Position:= DataOffset;
       iPrevPercent:= i;
     end;

  until EndOfFile Or (ErrorCode > 0);
  if TffFrames.GetFrameRecords = Nil then ErrorCode:= WRONG_FILE_FORMAT;
  TffStructure.SetSamplesTo1;
  Result:= TffFrames.GetFrameRecords;
  TffFrames.Done;

  wEndTime:= Time;
  App.TimeDifference.Caption:= IntToStr(SecondsBetween(wStartTime, wEndTime));

end;

end.

