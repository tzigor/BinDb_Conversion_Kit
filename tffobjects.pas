unit TffObjects;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  Utils, UserTypes;

type
  TTffStructure = object
  private
     DataChannelSize : Word;
     NumberOfChannels: Word;
     TFFDataChannels : TTFFDataChannels;
     CurrentOffset   : Word;
  public
     constructor Init;
     destructor Done;
     function GetDataChannelSize: Word;
     function GetNumberOfChannels: Word;
     function GetTFFDataChannels: TTFFDataChannels;
     function GetOffsetByName(DLISname: String): Word;
     procedure AddChannel(DLIS, Units, RepCode, Samples: String; TffVersion: Byte);
     function GetChannel(Index: Word): TTFFDataChannel;
  end;

  TTffFrames = object
  private
     FrameRecords: TFrameRecords;
     NumberOfRecords: longWord;
  public
     constructor Init;
     destructor Done;
     function GetCurrentFrameRecord: TFrameRecord;
     function GetFrameRecords: TFrameRecords;
     procedure AddRecord(DateTime: TDateTime; Size: Word; TFFDataChannels: TTFFDataChannels);
     procedure AddData(Index: Word; Data: ShortInt);
     procedure AddData(Index: Word; Data: Byte);
     procedure AddData(Index: Word; Data: SmallInt);
     procedure AddData(Index: Word; Data: Word);
     procedure AddData(Index: Word; Data: LongInt);
     procedure AddData(Index: Word; Data: LongWord);
     procedure AddData(Index: Word; Data: Single);
     procedure AddData(Index: Word; Data: Double);
  end;

implementation

  constructor TTffStructure.Init;
  begin
     DataChannelSize:= 0;
     NumberOfChannels:= 0;
     CurrentOffset:= 0;
     SetLength(TffDataChannels, 0);
  end;

  destructor TTffStructure.Done;
  begin
     SetLength(TffDataChannels, 0);
  end;

  function TTffStructure.GetDataChannelSize: Word;
  begin
     Result:= DataChannelSize;
  end;

  function TTffStructure.GetNumberOfChannels: Word;
  begin
     Result:= NumberOfChannels;
  end;

  function TTffStructure.GetTFFDataChannels: TTFFDataChannels;
  begin
     Result:= TFFDataChannels;
  end;

  function TTffStructure.GetOffsetByName(DLISname: String): Word;
  var i, Offset: Word;
  begin
     Offset:= U2_RESULT_ERROR;
     for i:= 0 to NumberOfChannels - 1 do
        if TFFDataChannels[i].DLIS = DLISname then begin
           Offset:= TFFDataChannels[i].Offset;
           Break;
        end;
     Result:= Offset;
  end;

  procedure TTffStructure.AddChannel(DLIS, Units, RepCode, Samples: String; TffVersion: Byte);
  var TffDataChannel: TTffDataChannel;
  begin
    TffDataChannel.DLIS:= Trim(DLIS);
    TffDataChannel.Samples:= Trim(Samples);
    TffDataChannel.Units:= Trim(Units);
    TffDataChannel.RepCode:= Trim(RepCode);
    case LowerCase(RepCode) of
       'f4', 'f8': TffDataChannel.AbsentValue:= '-999.25';
       'i1'      : TffDataChannel.AbsentValue:= '127';
       'u1'      : TffDataChannel.AbsentValue:= '255';
       'i2'      : TffDataChannel.AbsentValue:= '32767';
       'u2'      : TffDataChannel.AbsentValue:= '65535';
       'u4'      : TffDataChannel.AbsentValue:= '4294967295';
       'i4'      : TffDataChannel.AbsentValue:= '2147483647';
    end;
    TffDataChannel.Offset:= CurrentOffset;
    if Not (UpperCase(DLIS) = 'TIME') then
       case LowerCase(RepCode) of
         'f8'            : Inc(CurrentOffset, 8);
         'f4', 'u4', 'i4': Inc(CurrentOffset, 4);
         'i2', 'u2'      : Inc(CurrentOffset, 2);
         'i1', 'u1'      : Inc(CurrentOffset, 1);
       end;
    DataChannelSize:= DataChannelSize + StrToInt(Copy(RepCode, 2, 1));
    Insert(TffDataChannel, TffDataChannels, NumberOfChannels + 1);
    Inc(NumberOfChannels);
  end;

  function TTffStructure.GetChannel(Index: Word): TTffDataChannel;
  begin
    if Index < NumberOfChannels then Result:= TffDataChannels[Index];
  end;


 // TTffFrames ////////////////////////////////////////////////////////////////

  constructor TTffFrames.Init();
  begin
     NumberOfRecords:= 0;
     SetLength(FrameRecords, 0);
  end;

  destructor TTffFrames.Done;
  begin
    SetLength(FrameRecords, 0);
  end;

  function TTffFrames.GetCurrentFrameRecord: TFrameRecord;
  begin
    Result:= FrameRecords[NumberOfRecords - 1];
  end;

  function TTffFrames.GetFrameRecords: TFrameRecords;
  begin
    Result:= FrameRecords;
  end;

  procedure TTffFrames.AddData(Index: Word; Data: ShortInt);
  begin
    if Index <> U2_RESULT_ERROR then Move(Data, FrameRecords[NumberOfRecords - 1].Data[Index], 1);
  end;

  procedure TTffFrames.AddData(Index: Word; Data: Byte);
  begin
     if Index <> U2_RESULT_ERROR then Move(Data, FrameRecords[NumberOfRecords - 1].Data[Index], 1);
  end;

  procedure TTffFrames.AddData(Index: Word; Data: SmallInt);
  begin
     if Index <> U2_RESULT_ERROR then Move(Data, FrameRecords[NumberOfRecords - 1].Data[Index], 2);
  end;

  procedure TTffFrames.AddData(Index: Word; Data: Word);
  begin
     if Index <> U2_RESULT_ERROR then Move(Data, FrameRecords[NumberOfRecords - 1].Data[Index], 2);
  end;

  procedure TTffFrames.AddData(Index: Word; Data: LongInt);
  begin
     if Index <> U2_RESULT_ERROR then Move(Data, FrameRecords[NumberOfRecords - 1].Data[Index], 4);
  end;

  procedure TTffFrames.AddData(Index: Word; Data: LongWord);
  begin
     if Index <> U2_RESULT_ERROR then Move(Data, FrameRecords[NumberOfRecords - 1].Data[Index], 4);
  end;

  procedure TTffFrames.AddData(Index: Word; Data: Single);
  begin
     if Index <> U2_RESULT_ERROR then Move(Data, FrameRecords[NumberOfRecords - 1].Data[Index], 4);
  end;

  procedure TTffFrames.AddData(Index: Word; Data: Double);
  begin
     if Index <> U2_RESULT_ERROR then Move(Data, FrameRecords[NumberOfRecords - 1].Data[Index], 8);
  end;

  procedure TTffFrames.AddRecord(DateTime: TDateTime; Size: Word; TFFDataChannels: TTFFDataChannels);
  var FrameRecord: TFrameRecord;
      NumOfChannels, i: Word;
  begin
    FrameRecord.DateTime:= DateTime;
    SetLength(FrameRecord.Data, Size - 4); // minus 4 bytes for TIME
    Insert(FrameRecord, FrameRecords, NumberOfRecords + 1);
    Inc(NumberOfRecords);
    NumOfChannels:= Length(TFFDataChannels);
    for i:=1 to NumOfChannels - 1 do begin
       case LowerCase(TFFDataChannels[i].RepCode) of
          'i1': AddData(TFFDataChannels[i].Offset, 127);
          'u1': AddData(TFFDataChannels[i].Offset, 255);
          'i2': AddData(TFFDataChannels[i].Offset, 32767);
          'u2': AddData(TFFDataChannels[i].Offset, 65535);
          'u4': AddData(TFFDataChannels[i].Offset, 4294967295);
          'i4': AddData(TFFDataChannels[i].Offset, 2147483647);
          'f4', 'f8': AddData(TFFDataChannels[i].Offset, -999.25);
       end;
    end;
  end;


end.


