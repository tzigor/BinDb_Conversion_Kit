unit BIN_DB_Converter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  Utils, UserTypes, TffObjects;

type
  TBinDbConverter = object
  private
     TFFVersion      : Byte;
     BinDbData       : TBytes;
     DataOffset      : longWord;
     FirstDateTime   : TDateTime; // Acquisition Start Date/Time
     TimeShift       : Single;
     TimeShiftByUser : Int16;
     MRL             : Word;
  public
     constructor Init(Version: Byte; FrameRecords: TFrameRecords);
     destructor Done;
     function GetBinDbData: TBytes;
     procedure SetTimeShiftByUser(hr, min, sec: Int16);
     procedure AddLength(Len: LongWord);
     procedure CreateParameters();
     procedure AddParameter(Param: String);
     procedure ChannelsComposer(Channels: TTFFDataChannels);
     procedure FramesComposer(FrameRecords: TFrameRecords);
     procedure Composer(Channels: TTFFDataChannels; FrameRecords: TFrameRecords);
  end;

implementation

constructor TBinDbConverter.Init(Version: Byte; FrameRecords: TFrameRecords);
begin
  SetLength(BinDbData, 0);
  DataOffset:= 0;
  TimeShiftByUser:= 0;
  TFFVersion:= Version;
  MRL:= Length(FrameRecords[0].Data) + 4 + 1; { + 4 bytes for time + 'F' }
  FirstDateTime:= FrameRecords[0].DateTime;
  TimeShift:= SecondsBetween(FirstDateTime, DateOf(FirstDateTime)) / 100;
end;

destructor TBinDbConverter.Done;
begin
  SetLength(BinDbData, 0);
end;

function TBinDbConverter.GetBinDbData: TBytes;
begin
  Result:= BinDbData;
end;

procedure TBinDbConverter.SetTimeShiftByUser(hr, min, sec: Int16);
begin
  TimeShiftByUser:= hr * 3600 + min * 60 + sec;
  FirstDateTime:= IncSecond(FirstDateTime, TimeShiftByUser);
  TimeShift:= SecondsBetween(FirstDateTime, DateOf(FirstDateTime)) / 100;
end;

procedure TBinDbConverter.AddLength(Len: LongWord);
begin
  if Len < 65536 then begin
     Insert(Len And $00FF, BinDbData, DATA_MAX_SIZE);
     Insert(Len >> 8, BinDbData, DATA_MAX_SIZE);
  end
  else begin
     Insert(Len And $000000FF, BinDbData, DATA_MAX_SIZE);
     Insert((Len >> 8) And $0000FF, BinDbData, DATA_MAX_SIZE);
     Insert(Ord('L'), BinDbData, DATA_MAX_SIZE);
     Insert((Len >> 16) And $00FF, BinDbData, DATA_MAX_SIZE);
     Insert(Len >> 24, BinDbData, DATA_MAX_SIZE);
  end;
end;

procedure TBinDbConverter.CreateParameters();
begin
  if TFFVersion = TFF_V20 then AddParameter('FFV=V2.0')
  else if TFFVersion = TFF_V30 then AddParameter('FFV=V3.0')
       else if TFFVersion = TFF_V40 then AddParameter('FFV=V4.0');
  AddParameter('MRL=' + IntToStr(MRL));
  AddParameter('Acquisition Start Date=' + FormatDateTime('DD-MMM-YYYY', FirstDateTime));
  AddParameter('Acquisition Start Time=' + FormatDateTime('hh:nn:ss', FirstDateTime));
end;

procedure TBinDbConverter.AddParameter(Param: String);
var i, ParamSize: Word;
begin
  ParamSize:= Length(Param) + 2; { + 'P' + 0-(terminator) }
  AddLength(ParamSize);
  Insert(Ord('P'), BinDbData, DATA_MAX_SIZE);
  for i:=1 to ParamSize - 1 do Insert(Ord(Param[i]), BinDbData, DATA_MAX_SIZE);
end;

procedure TBinDbConverter.ChannelsComposer(Channels: TTFFDataChannels);
var j, ChannelLen, DLISLen, Units, Samples, RepCode, AbsentValue: Byte;
    NumOfChannels, i: Word;
begin
  NumOfChannels:= Length(Channels);
  DLISLen:= 10;
  Units:= 4;
  RepCode:= 2;
  Samples:= 10;
  AbsentValue:= 20;
  case TFFVersion of
     TFF_V20: begin
                 ChannelLen:= 41;
                 Samples:= 4;
              end;
     TFF_V30: begin
                 ChannelLen:= 47;
              end;
     TFF_V40: begin
                 ChannelLen:= 53;
                 DLISLen:= 16;
              end;
  end;
  for i:=0 to NumOfChannels - 1 do begin
     AddLength(ChannelLen);
     Insert(Ord('D'), BinDbData, DATA_MAX_SIZE);

     for j:=1 to DLISLen do begin
       if j <= Length(Channels[i].DLIS) then Insert(Ord(Channels[i].DLIS[j]), BinDbData, DATA_MAX_SIZE)
       else Insert(0, BinDbData, DATA_MAX_SIZE)
     end;

     for j:=1 to Units do begin
       if j <= Length(Channels[i].Units) then Insert(Ord(Channels[i].Units[j]), BinDbData, DATA_MAX_SIZE)
       else Insert(0, BinDbData, DATA_MAX_SIZE)
     end;

     for j:=1 to RepCode do begin
       if j <= Length(Channels[i].RepCode) then Insert(Ord(Channels[i].RepCode[j]), BinDbData, DATA_MAX_SIZE)
       else Insert(0, BinDbData, DATA_MAX_SIZE)
     end;

     for j:=1 to Samples do begin
       if j <= Length(Channels[i].Samples) then Insert(Ord(Channels[i].Samples[j]), BinDbData, DATA_MAX_SIZE)
       else Insert(0, BinDbData, DATA_MAX_SIZE)
     end;

     for j:=1 to AbsentValue do begin
       if j <= Length(Channels[i].AbsentValue) then Insert(Ord(Channels[i].AbsentValue[j]), BinDbData, DATA_MAX_SIZE)
       else Insert(0, BinDbData, DATA_MAX_SIZE)
     end;
  end;
end;

procedure TBinDbConverter.FramesComposer(FrameRecords: TFrameRecords);
var i, NumOfFrames: longWord;
    FrameLen, j: Word;
    F4: Single;
    F4Array: array[0..3] of Byte;
begin
  NumOfFrames:= Length(FrameRecords);
  for i:=0 to NumOfFrames - 1 do begin
     FrameLen:= Length(FrameRecords[i].Data);
     AddLength(FrameLen + 4 + 1); { + 4 bytes for time + 'F' }
     Insert(Ord('F'), BinDbData, DATA_MAX_SIZE);
     F4:= (SecondsBetween(FirstDateTime, IncSecond(FrameRecords[i].DateTime, TimeShiftByUser)) / 100) + TimeShift;
     Move(F4, F4Array, 4);
     for j:=0 to 3 do Insert(F4Array[j], BinDbData, DATA_MAX_SIZE);
     for j:=0 to FrameLen - 1 do Insert(FrameRecords[i].Data[j], BinDbData, DATA_MAX_SIZE);
  end;
end;

procedure TBinDbConverter.Composer(Channels: TTFFDataChannels; FrameRecords: TFrameRecords);
begin
  ChannelsComposer(Channels);
  FramesComposer(FrameRecords);
end;

end.


