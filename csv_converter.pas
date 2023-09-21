unit CSV_Converter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  UserTypes, Utils, StrUtils;

type
  TMeasurement = record
    ChannelName: String16;
    Sensor: String16;
    Phase: String16;
    MeasurementType: String32;
    MeasurementDescription: String[40];
    MeasurementValue: Single;
    MeasurementTime: longWord;
    Reference: Single;
    ReferenceLowTolerance: Single;
    ReferenceHighTolerance: Single;
    Limit: Single;
  end;

  TDataChannel = record
    DLISName: String16;
    Units: String4;
    RepCode: String2;
  end;

  TDataChannels = array of TDataChannel;

  TCSVConverter = object
  private
     CSVSeparator    : String;
     Data            : TBytes;
     ParamChannels   : TStringList;
     Measurements    : TStringList;
     CSVData         : TStringList;
     StartDate       : TDateTime;
     IncludeParams   : Boolean;
     IncludeMeasures : Boolean;
     AddUnits        : Boolean;
     AddType         : Boolean;
     DateTimeType    : Byte; { 1 - Human Date/Time, 2 - Unix Time, 3 - 100s }
     ItemLength      : Byte; { length of individual parameters for txt conversion }
  public
     constructor Init(Separator: String; IncludeP, IncludeM, AddU, AddT: Boolean; DateType: Byte);
     destructor Done;
     procedure SetIncludeParams(Value: Boolean);
     procedure SetIncludeMeasures(Value: Boolean);
     procedure SetAddUnits(Value: Boolean);
     procedure SetAddType(Value: Boolean);
     procedure SetDateTimeType(Value: Byte);
     procedure SetItemLength(Value: Byte);
     function GetCSVData(): TStringList;
     function GetRecordLength(): LongWord;
     procedure GetData(Size: LongWord);
     function DataToStr(): String;
     procedure FindStartDate(Parameter: String);
     function ParseMeasurement(): TMeasurement;
     function MeasurementToStr(Measurement: TMeasurement): String;
     function ParseDataChannel(RecordLength: longWord): TDataChannel;
     function ParseFrame(ChannelsList: TDataChannels): String;
     procedure CSVComposer();
  end;

implementation

uses Main;

  constructor TCSVConverter.Init(Separator: String; IncludeP, IncludeM, AddU, AddT: Boolean; DateType: Byte);
  begin
     CSVSeparator:= Separator;
     if ParamChannels is TStringList then FreeAndNil(ParamChannels);
     ParamChannels:= TStringList.Create;

     if Measurements is TStringList then FreeAndNil(Measurements);
     Measurements:= TStringList.Create;

     if CSVData is TStringList then FreeAndNil(CSVData);
     CSVData:= TStringList.Create;

     StartDate:= 0;
     IncludeParams:= IncludeP;
     IncludeMeasures:= IncludeM;
     AddUnits:= AddU;
     AddType:= AddT;
     DateTimeType:= DateType;
     ItemLength:= 12;
  end;

  destructor TCSVConverter.Done;
  begin
  end;

  procedure TCSVConverter.SetIncludeParams(Value: Boolean);
  begin
     IncludeParams:= Value;
  end;

  procedure TCSVConverter.SetIncludeMeasures(Value: Boolean);
  begin
     IncludeMeasures:= Value;
  end;

  procedure TCSVConverter.SetAddUnits(Value: Boolean);
  begin
     AddUnits:= Value;
  end;

  procedure TCSVConverter.SetAddType(Value: Boolean);
  begin
     AddType:= Value;
  end;

  procedure TCSVConverter.SetDateTimeType(Value: Byte);
  begin
     DateTimeType:= Value;
  end;

  procedure TCSVConverter.SetItemLength(Value: Byte);
  begin
     ItemLength:= Value;
  end;

  function TCSVConverter.GetCSVData(): TStringList;
  var i: Word;
  begin
     if IncludeMeasures And (Measurements.Count > 0) then begin
        for i:=Measurements.Count - 1 downto 0 do CSVData.Insert(0, Measurements[i]);
     end;

     if IncludeParams And (ParamChannels.Count > 0) then begin
        for i:=ParamChannels.Count - 1 downto 0 do CSVData.Insert(0, ParamChannels[i]);
     end;

     Result:= CSVData;
  end;

  function TCSVConverter.DataToStr(): String;
  var len, i: Word;
      wStr: String;
  begin
    wStr:= '';
    len:= length(Data);
    for i:=1 to len do
      if Data[i-1] > 0 then wStr:= wStr + Chr(Data[i-1]);
    Result:= wStr;
  end;

  procedure TCSVConverter.FindStartDate(Parameter: String);
  begin
     if (NPos('START', UpperCase(Parameter), 1) > 0) And
              (NPos('DATE', UpperCase(Parameter), 1) > 0) then begin
        Copy2SymbDel(Parameter, '=');
        try
          StartDate:= ScanDateTime('dd-mmm-yyyy', DelSpace(Parameter));
        except
          on Exception : EConvertError do
        end;
     end;
  end;

  function TCSVConverter.GetRecordLength(): LongWord;
  var RecordLength : LongWord;
      RecordType   : Char;
  begin
    RecordLength:= ReadCurrentByte;
    RecordLength:= (RecordLength or (ReadCurrentByte << 8)) - 1;
    RecordType:= Chr(ReadCurrentByte);
    if RecordType = 'L' then begin
        RecordLength:= RecordLength or (ReadCurrentByte << 16);
        RecordLength:= (RecordLength or (ReadCurrentByte << 24)) - 1;
    end;
    Dec(DataOffset);
    Result:= RecordLength;
  end;

  procedure TCSVConverter.GetData(Size: LongWord);
  var i: longWord;
  begin
    setLength(Data, Size);
    for i:= 0 to Size - 1 do Data[i]:= ReadCurrentByte;
  end;

  function TCSVConverter.ParseMeasurement(): TMeasurement;
  var i, n: Byte;
      Measurement: TMeasurement;
  begin
    with Measurement do begin
       ChannelName:= '';
       Sensor:= '';
       Phase:= '';
       MeasurementType:= '';

       for i:=0 to 15 do if Data[i] > 0 then ChannelName:= ChannelName + Chr(Data[i]);
       for i:=0 to 15 do if Data[i+16] > 0 then Sensor:= Sensor + Chr(Data[i+16]);
       for i:=0 to 15 do if Data[i+32] > 0 then Phase:= Phase + Chr(Data[i+32]);
       for i:=0 to 31 do if Data[i+48] > 0 then MeasurementType:= MeasurementType + Chr(Data[i+48]);
       for i:=0 to 39 do if Data[i+80] > 0 then MeasurementDescription:= MeasurementDescription + Chr(Data[i+80]);

       Move(Data[120], MeasurementValue, 4);
       Move(Data[124], MeasurementTime, 4);
       Move(Data[128], Reference, 4);
       Move(Data[132], ReferenceLowTolerance, 4);
       Move(Data[136], ReferenceHighTolerance, 4);
       Move(Data[141], Limit, 4);
    end;
    Result:= Measurement;
  end;

  function TCSVConverter.MeasurementToStr(Measurement: TMeasurement): String;
  var wStr: String;
  begin
    wStr:= '';
    with Measurement do begin
       wStr:= wStr + ChannelName + ' ' + Sensor + ' ' + Phase + ' ' + MeasurementType + ' ' + MeasurementDescription + ' ';
       wStr:= wStr + FloatToStrF(MeasurementValue, ffFixed, 10, 2) + ' ';
       wStr:= wStr + IntToStr(MeasurementTime) + ' ';
       wStr:= wStr + FloatToStrF(MeasurementValue, ffFixed, 10, 2) + ' ';
       wStr:= wStr + FloatToStrF(Reference, ffFixed, 10, 2) + ' ';
       wStr:= wStr + FloatToStrF(ReferenceLowTolerance, ffFixed, 10, 2) + ' ';
       wStr:= wStr + FloatToStrF(ReferenceHighTolerance, ffFixed, 10, 2) + ' ';
       wStr:= wStr + FloatToStrF(Limit, ffFixed, 10, 2) + NewLine;
    end;
    Result:= wStr;
  end;

  function TCSVConverter.ParseDataChannel(RecordLength: longWord): TDataChannel;
  var DataChannel: TDataChannel;
    i, DLISNameLen, UnitsLen, RepCodeLen, Shift: Byte;
  begin
    DLISNameLen:= 10;
    UnitsLen:= 4;
    RepCodeLen:= 2;
    if RecordLength >= 52 then DLISNameLen:= 16;
    with DataChannel do begin
       DLISName:= '';
       Units:= '';
       RepCode:= '';
       Shift:= 0;
       for i:= 0 to DLISNameLen - 1 do if Data[i] > 0 then DLISName:= DLISName + Chr(Data[i]);
       Shift:= Shift + DLISNameLen;
       for i:= 0 to UnitsLen - 1 do if Data[i+Shift] > 0 then Units:= Units + Chr(Data[i+Shift]);
       Shift:= Shift + UnitsLen;
       for i:= 0 to RepCodeLen - 1 do if Data[i+Shift] > 0 then RepCode:= RepCode + Chr(Data[i+Shift]);
    end;
    Result:= DataChannel;
  end;

  function TCSVConverter.ParseFrame(ChannelsList: TDataChannels): String;
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
                  Move(Data[DataCount], F4, 4);
                  if i = 0 then begin  { if TIME channel }
                     if DateTimeType = 1 then FrameStr:= DateTimeToStr(IncMilliSecond(StartDate, Round(F4 * 100000 )))
                     else if DateTimeType = 2 then FrameStr:= IntToStr(DateTimeToUnix(IncMilliSecond(StartDate, Round(F4 * 100000 ))))
                          else FrameStr:= FloatToStrF(F4, ffFixed, 10, App.FloatDigits.Value);
                  end
                  else FrameStr:= FloatToStrF(F4, ffFixed, 10, App.FloatDigits.Value);
                  Inc(DataCount, 4);
                end;
          'F8': begin
                  Move(Data[DataCount], F8, 8);
                  FrameStr:= FloatToStrF(F8, ffFixed, 10, App.FloatDigits.Value);
                  Inc(DataCount, 8);
                end;
          'I1': begin
                  I1:= Data[DataCount];
                  FrameStr:= IntToStr(I1);
                  Inc(DataCount);
                end;
          'U1': begin
                  U1:= Data[DataCount];
                  FrameStr:= IntToStr(U1);
                  Inc(DataCount);
                end;
          'I2': begin
                   Move(Data[DataCount], I2, 2);
                   FrameStr:= IntToStr(I2);
                   Inc(DataCount, 2);
                end;
          'U2': begin
                   Move(Data[DataCount], U2, 2);
                   FrameStr:= IntToStr(U2);
                   Inc(DataCount, 2);
                end;
          'U4': begin
                   Move(Data[DataCount], U4, 2);
                   FrameStr:= IntToStr(U4);
                   Inc(DataCount, 4);
                end;
          'I4': begin
                   Move(Data[DataCount], I4, 2);
                   FrameStr:= IntToStr(I4);
                   Inc(DataCount, 4);
                end;
          'U8': begin
                   Move(Data[DataCount], U8, 8);
                   FrameStr:= IntToStr(U8);
                   Inc(DataCount, 8);
                end
        end;
        if CSVSeparator <> '' then Frame:= Frame + FrameStr + CSVSeparator
        else if i = 0 then Frame:= Frame + SetStringLength(FrameStr, 20) { convert to TXT} { i=0 mean TIME }
             else Frame:= Frame + SetStringLength(FrameStr, ItemLength)
     end;
     Result:= Frame;
  end;

  procedure TCSVConverter.CSVComposer();
  var RecordLength   : LongWord;
      RecordType     : Char;
      isFirstFrame   : Boolean;
      Channels       : String;
      NewParameter   : String;
      DataChannels   : TDataChannels;
      ChannelsCount  : Word;
      CurrentChannel : String;
  begin
    Channels:= '';
    ChannelsCount:= 0;
    ErrorCode:= 0;
    isFirstFrame:= True;
    repeat
       RecordLength:= GetRecordLength;
       RecordType:= Chr(ReadCurrentByte);
       GetData(RecordLength);
       case RecordType of
          'P': begin
                  NewParameter:= DataToStr;
                  ParamChannels.Add(NewParameter);
                  FindStartDate(NewParameter); // Try to find Start Aquisition Date
               end;
          'M': Measurements.Add(MeasurementToStr(ParseMeasurement));
          'D': begin
                  Insert(ParseDataChannel(RecordLength), DataChannels, DATA_MAX_SIZE);
                  CurrentChannel:= DataChannels[ChannelsCount].DLISName;
                  if AddUnits then CurrentChannel:= CurrentChannel + '(' + DataChannels[ChannelsCount].Units + ')';
                  if AddType then CurrentChannel:= CurrentChannel + '(' + DataChannels[ChannelsCount].RepCode + ')';

                  if CSVSeparator <> '' then Channels:= Channels + CurrentChannel + CSVSeparator
                  else if ChannelsCount = 0 then Channels:= Channels + SetStringLength(CurrentChannel, 20) { convert to TXT} { ChannelsCount=0 mean TIME }
                       else Channels:= Channels + SetStringLength(CurrentChannel, ItemLength);

                  Inc(ChannelsCount);
               end;
          'F': begin
                  if isFirstFrame then CSVData.Add(Channels);
                  isFirstFrame:= False;
                  CSVData.Add(ParseFrame(DataChannels));
               end;
          'B':;
       else ErrorCode:= WRONG_FILE_FORMAT;
       end;
    until EndOfFile Or (ErrorCode > 0);
  end;

end.

