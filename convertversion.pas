unit ConvertVersion;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  Utils, UserTypes, StrUtils;

procedure ConvertBinDbVersion();

implementation
uses Main;

type

TDataChannel = record
  DLISName: String16;
  Units: String4;
  RepCode: String2;
  Samples: LongInt;
  AbsentValue: String20;
end;

TFrame = array of String;

var
  OutBytes: array of Byte;
  OutCounter: longWord;
  DataChannels: array of TDataChannel;

  function ParseDataChannel(RecordLength: longWord): TDataChannel;
  var DataChannel: TDataChannel;
      i, DLISNameLen, UnitsLen, RepCodeLen, SamplesLen, AbsentValueLen: Byte;
      StrSamples: String;
      b: Byte;
  begin
    DLISNameLen:= 10;
    UnitsLen:= 4;
    RepCodeLen:= 2;
    SamplesLen:= 10;
    if RecordLength < 42 then SamplesLen:= 4;
    if RecordLength >= 52 then DLISNameLen:= 16;
    AbsentValueLen:= RecordLength - DLISNameLen - UnitsLen - RepCodeLen - SamplesLen;
    with DataChannel do begin
       DLISName:= '';
       Units:= '';
       RepCode:= '';
       StrSamples:= '';
       AbsentValue:= '';
       for i:= 0 to DLISNameLen - 1 do begin
          b:= ReadCurrentByte;
          if b > 0 then DLISName:= DLISName + Chr(b)
          else DLISName:= DLISName + ' ';
       end;

       for i:= 0 to UnitsLen - 1 do begin
          b:= ReadCurrentByte;
          if b > 0 then Units:= Units + Chr(b)
          else Units:= Units + ' ';
       end;

       for i:= 0 to RepCodeLen - 1 do begin
          b:= ReadCurrentByte;
          if b > 0 then RepCode:= RepCode + Chr(b)
          else RepCode:= RepCode + ' ';
       end;

       for i:= 0 to SamplesLen - 1 do begin
          b:= ReadCurrentByte;
          if b > 0 then StrSamples:= StrSamples + Chr(b);
       end;
       TryStrToInt(Trim(StrSamples), Samples);

       for i:= 0 to AbsentValueLen - 1 do begin
          b:= ReadCurrentByte;
          if b > 0 then AbsentValue:= AbsentValue + Chr(b)
          else AbsentValue:= AbsentValue + ' ';
       end;
    end;
    Result:= DataChannel;
  end;

  procedure ComposeDataChannel(DataChannel: TDataChannel);
  var i, DLISNameLen, UnitsLen, RepCodeLen, SamplesLen, AbsentValueLen, ChannelLen: Byte;
      sDLISNameLen, sUnitsLen, sRepCodeLen, sSamplesLen, sAbsentValueLen: Byte;
      wDLISName, wUnits, wRepCode, wSamples, wAbsentValue: String;
  begin
    DLISNameLen:= 10;
    UnitsLen:= 4;
    RepCodeLen:= 2;
    SamplesLen:= 10;
    AbsentValueLen:= 20;

    case TFFVersion of
       TFF_V20: begin
                   ChannelLen:= 41;
                   SamplesLen:= 4;
                end;
       TFF_V30: begin
                   ChannelLen:= 47;
                end;
       TFF_V40: begin
                   ChannelLen:= 53;
                   DLISNameLen:= 16;
                end;
    end;

    OutBytes[OutCounter]:= ChannelLen;
    Inc(OutCounter);
    OutBytes[OutCounter]:= 0;
    Inc(OutCounter);
    OutBytes[OutCounter]:= Ord('D');
    Inc(OutCounter);

    with DataChannel do begin
       wDLISName:= Trim(DLISName);
       if wDLISName = 'STATUS.SIBR.LO' then wDLISName:= 'STATUS.LO';
       if wDLISName = 'STATUS.SIBR.HI' then wDLISName:= 'STATUS.HI';
       if wDLISName = 'ESTATUS.SIBR.LO' then wDLISName:= 'ESTATUS.LO';
       if wDLISName = 'ESTATUS.SIBR.HI' then wDLISName:= 'ESTATUS.HI';
       wUnits:= Trim(Units);
       wRepCode:= Trim(RepCode);
       wSamples:= Trim(IntToStr(Samples));
       wAbsentValue:= Trim(AbsentValue);
       if wAbsentValue = '7FFFFFFF' then wAbsentValue:='4294967295';

       sDLISNameLen:= length(wDLISName);
       sUnitsLen:= length(wUnits);
       sRepCodeLen:= length(wRepCode);
       sSamplesLen:= length(wSamples);
       sAbsentValueLen:= length(wAbsentValue);

       for i:= 1 to DLISNameLen do begin
          if (sDLISNameLen >= i) And (wDLISName <> '') then begin
             OutBytes[OutCounter]:= Ord(wDLISName[i]);
          end
          else OutBytes[OutCounter]:= 0;
          Inc(OutCounter);
       end;
       for i:= 1 to UnitsLen do begin
          if (sUnitsLen >= i) And (wUnits <> '') then OutBytes[OutCounter]:= Ord(wUnits[i])
          else OutBytes[OutCounter]:= 0;
          Inc(OutCounter);
       end;
       for i:= 1 to RepCodeLen do begin
          if (sRepCodeLen >= i) And (wRepCode <> '') then OutBytes[OutCounter]:= Ord(wRepCode[i])
          else OutBytes[OutCounter]:= 0;
          Inc(OutCounter);
       end;
       for i:= 1 to SamplesLen do begin
          if (sSamplesLen >= i) And (wSamples <> '') then OutBytes[OutCounter]:= Ord(wSamples[i])
          else OutBytes[OutCounter]:= 0;
          Inc(OutCounter);
       end;
       for i:= 1 to AbsentValueLen do begin
          if (sAbsentValueLen >= i) And (wAbsentValue <> '') then OutBytes[OutCounter]:= Ord(wAbsentValue[i])
          else OutBytes[OutCounter]:= 0;
          Inc(OutCounter);
       end;
    end;
  end;

  procedure CopyRecord(RecType: Char; RecordLength: Word);
  var i, StartIndex: Word;
  begin
    OutBytes[OutCounter]:= (RecordLength + 1) And $00FF;
    Inc(OutCounter);
    OutBytes[OutCounter]:= (RecordLength + 1) >> 8;
    Inc(OutCounter);
    OutBytes[OutCounter]:= Ord(RecType);
    Inc(OutCounter);
    StartIndex:= 0;

    for i:=StartIndex to RecordLength - 1 do begin
      OutBytes[OutCounter]:= ReadCurrentByte;
      Inc(OutCounter);
    end;
  end;

  function GetDateTimeMillis(): longWord;  { Milli seconds }
  var F4: Single;
      Data: array[0..3] of Byte;
  begin
    Data[0]:= ReadCurrentByte;
    Data[1]:= ReadCurrentByte;
    Data[2]:= ReadCurrentByte;
    Data[3]:= ReadCurrentByte;
    Move(Data[0], F4, 4);
    Result:= Round(F4 * 100000 );
    Dec(DataOffset, 4);
  end;

  procedure ConvertBinDbVersion();
  var RecordLength: Word;
      RecordType: Char;
      FileCheck: String4;
      Milliseconds: LongWord;
      PrevMilliseconds: LongWord;
      Diff, RecordRateValue: LongWord;
  begin
    if LoadSourceFile('bin_db', 100) then begin
       EndOfFile:= False;
       ErrorCode:= NO_ERROR;
       PrevMilliseconds:= 0;
       setLength(DataChannels, 0);
       RecordRateValue:= App.RecordRate.Value;
       if CurrentFileSize > 1000 then FileCheck:= Chr(Bytes[2]) + Chr(Bytes[3]) + Chr(Bytes[4]) + Chr(Bytes[5]);
       if FileCheck = 'PFFV' then begin
         Bytes[8]:= Ord(IntToStr(TFFVersion)[1]);
         SetLength(OutBytes, CurrentFileSize);
         DataOffset:= 0;
         OutCounter:= 0;
         repeat
            RecordLength:= ReadCurrentByte;
            RecordLength:= (RecordLength or (ReadCurrentByte shl 8)) - 1;
            RecordType:= Chr(ReadCurrentByte);
            if RecordType = 'P' then CopyRecord('P', RecordLength)
            else if RecordType = 'M' then CopyRecord('M', RecordLength)
                 else if RecordType = 'F' then begin
                          Milliseconds:= GetDateTimeMillis;
                          Diff:= Milliseconds - PrevMilliseconds + 10;
                          if Diff >= RecordRateValue then begin
                             CopyRecord('F', RecordLength);
                             PrevMilliseconds:= Milliseconds;
                          end
                          else Inc(DataOffset, RecordLength);
                      end
                      else if RecordType = 'B' then CopyRecord('B', RecordLength)
                           else if RecordType = 'D' then begin
                                   ComposeDataChannel(ParseDataChannel(RecordLength));
                                end;
         until EndOfFile Or (ErrorCode > 0);
         SetLength(OutBytes, OutCounter);
         SaveByteArray(OutBytes, ReplaceText(CurrentOpenedFile, ExtractFileExt(CurrentOpenedFile),'') + '_converted.bin_db');
       end
     end;
  end;

end.

