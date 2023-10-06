unit ParseBinDb;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  Utils, UserTypes, TffObjects, StrUtils;

function BinDbParser(Tff_Ver: Byte): TFrameRecords;

implementation
uses Main;

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
     if wSamples > 1 then DLIS:= DLIS + '[]';
     for i:= 0 to AbsentValueLen - 1 do begin
        b:= ReadCurrentByte;
        if b > 0 then AbsentValue:= AbsentValue + Chr(b);
     end;
  end;
  Result:= DataChannel;
end;

function ParseFrame(ChannelsList: TTFFDataChannels): String;
var i, ChannelsCount : Word;
    FrameStr, Frame  : String;
    DataCount        : longWord;
    I1               : ShortInt;
    U1               : Byte;
    I2               : SmallInt;
    U2               : Word;
    I4               : LongInt;
    U4               : LongWord;
    U8               : QWord;
    F4               : Single;
    F8               : Double;

begin
   DataCount:= 0;
   Frame:= '';
   ChannelsCount:= length(ChannelsList);
   for i:=0 to ChannelsCount - 1 do begin
      FrameStr:= '';
      case ChannelsList[i].RepCode of
        'F4': begin
                Move(Bytes[DataOffset], F4, 4);
                if i = 0 then begin  { if TIME channel }
                   if DateTimeType = 1 then FrameStr:= DateTimeToStr(IncMilliSecond(StartDate, Round(F4 * 100000 )))
                   else if DateTimeType = 2 then FrameStr:= IntToStr(DateTimeToUnix(IncMilliSecond(StartDate, Round(F4 * 100000 ))))
                        else FrameStr:= FloatToStrF(F4, ffFixed, 10, App.FloatDigits.Value);
                   IncDataOffset(4);
                end
                else begin
                   FrameStr:= FloatToStrF(F4, ffFixed, 10, App.FloatDigits.Value);
                   if (F4 = -999.25) or (ChannelsList[i].Samples = 1) then IncDataOffset(4)
                   else IncDataOffset(4 * ChannelsList[i].Samples);
                end;
              end;
        'F8': begin
                Move(Bytes[DataOffset], F8, 8);
                FrameStr:= FloatToStrF(F8, ffFixed, 10, App.FloatDigits.Value);
                if (F8 = -999.25) or (ChannelsList[i].Samples = 1) then IncDataOffset(8)
                else IncDataOffset(8 * ChannelsList[i].Samples);
              end;
        'I1': begin
                I1:= ReadCurrentByte;
                FrameStr:= IntToStr(I1);
                if (I1 <> 127) And (ChannelsList[i].Samples > 1) then IncDataOffset(ChannelsList[i].Samples - 1);
              end;
        'U1': begin
                U1:= ReadCurrentByte;
                FrameStr:= IntToStr(U1);
                if (I1 <> 255) And (ChannelsList[i].Samples > 1) then IncDataOffset(ChannelsList[i].Samples - 1);
              end;
        'I2': begin
                 Move(Bytes[DataOffset], I2, 2);
                 FrameStr:= IntToStr(I2);
                 if I2 = 32767 then IncDataOffset(2)
                 else IncDataOffset(2 * ChannelsList[i].Samples);
              end;
        'U2': begin
                 Move(Bytes[DataOffset], U2, 2);
                 FrameStr:= IntToStr(U2);
                 if (U2 = 65535) or (ChannelsList[i].Samples = 1) then IncDataOffset(2)
                 else IncDataOffset(2 * ChannelsList[i].Samples);
              end;
        'U4': begin
                 Move(Bytes[DataOffset], U4, 4);
                 FrameStr:= IntToStr(U4);
                 if (U4 = 4294967295) or (ChannelsList[i].Samples = 1) then IncDataOffset(4)
                 else IncDataOffset(4 * ChannelsList[i].Samples);
              end;
        'I4': begin
                 Move(Bytes[DataOffset], I4, 4);
                 FrameStr:= IntToStr(I4);
                 if (I4 = 2147483647) or (ChannelsList[i].Samples = 1) then IncDataOffset(4)
                 else IncDataOffset(4 * ChannelsList[i].Samples);
              end;
        'U8': begin
                 Move(Bytes[DataOffset], U8, 8);
                 FrameStr:= IntToStr(U8);
                 IncDataOffset(8);
              end
      end;
      if CSVSeparator <> '' then Frame:= Frame + FrameStr + CSVSeparator
      else if i = 0 then Frame:= Frame + SetStringLength(FrameStr, 20) { convert to TXT} { i=0 mean TIME }
           else Frame:= Frame + SetStringLength(FrameStr, ItemLength)
   end;
   Result:= Frame;
end;

function BinDbParser(Tff_Ver: Byte): TFrameRecords;
var
    RecordType     : Char;
    isFirstFrame   : Boolean;
    Channels       : String;
    NewParameter   : String;
    ChannelsCount  : Word;
    CurrentChannel : String;

    ParamChannels : TStringList;
    RecordLength  : LongWord;
    DateTime      : TDateTime;
    StartDate     : TDateTime;
    StartTime     : TDateTime;
    DataChannel   : TTFFDataChannel;
    TffFrames     : TTffFrames;

begin
  Channels:= '';
  ChannelsCount:= 0;
  ErrorCode:= NO_ERROR;
  isFirstFrame:= True;
  TffStructure.Init;
  ProgressInit(CurrentFileSize, 'Converting');
  if ParamChannels is TStringList then FreeAndNil(ParamChannels);
  ParamChannels:= TStringList.Create;
  repeat
     RecordLength:= GetRecordLength;
     RecordType:= Chr(ReadCurrentByte);
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
                Insert(ParseDataChannel(RecordLength), DataChannels, DATA_MAX_SIZE);
                Inc(ChannelsCount);
             end;
        'F': begin

             end;
        'B': IncDataOffset(RecordLength);
     else ErrorCode:= WRONG_FILE_FORMAT;
     end;
     App.ProcessProgress.Position:= DataOffset;
  until EndOfFile Or (ErrorCode > 0);
  Channels:= '';
end;

end.

