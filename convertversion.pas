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
  Samples: longInt;
  AbsentValue: String20;
end;

TFrame = array of String;

var
  OutBytes: array of Byte;
  Counter, OutCounter: longWord;
  Data: array of Byte;
  DataChannels: array of TDataChannel;

  procedure GetData(Len: longWord);
  var i: longWord;
  begin
    setLength(Data, Len);
    for i:= 1 to Len do begin
       Data[i-1]:= Bytes[Counter];
       Counter:= Counter + 1;
    end;
  end;

  function ParseDataChannel(RecordLength: longWord): TDataChannel;
  var DataChannel: TDataChannel;
      i, DLISNameLen, UnitsLen, RepCodeLen, SamplesLen, AbsentValueLen, Shift: Byte;
      StrSamples: String;
  begin
    DLISNameLen:= 10;
    UnitsLen:= 4;
    RepCodeLen:= 2;
    SamplesLen:= 10;
    AbsentValueLen:= 20;
    if RecordLength = 40 then SamplesLen:= 4;
    if RecordLength = 52 then DLISNameLen:= 16;

    with DataChannel do begin
       DLISName:= '';
       Units:= '';
       RepCode:= '';
       StrSamples:= '';
       AbsentValue:= '';
       Shift:= 0;
       for i:= 0 to DLISNameLen - 1 do if Data[i] > 0 then DLISName:= DLISName + Chr(Data[i])
                         else DLISName:= DLISName + ' ';
       Shift:= Shift + DLISNameLen;
       for i:= 0 to UnitsLen - 1 do if Data[i+Shift] > 0 then Units:= Units + Chr(Data[i+Shift])
                         else Units:= Units + ' ';
       Shift:= Shift + UnitsLen;
       for i:= 0 to RepCodeLen - 1 do if Data[i+Shift] > 0 then RepCode:= RepCode + Chr(Data[i+Shift])
                         else RepCode:= RepCode + ' ';
       Shift:= Shift + RepCodeLen;
       for i:= 0 to SamplesLen - 1 do if Data[i+Shift] > 0 then StrSamples:= StrSamples + Chr(Data[i+Shift]);
       TryStrToInt(Trim(StrSamples), Samples);
       Shift:= Shift + SamplesLen;
       for i:= 0 to AbsentValueLen - 1 do if Data[i+Shift] > 0 then AbsentValue:= AbsentValue + Chr(Data[i+Shift])
                          else AbsentValue:= AbsentValue + ' ';
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
          OutCounter:= OutCounter + 1;
       end;
       for i:= 1 to UnitsLen do begin
          if (sUnitsLen >= i) And (wUnits <> '') then OutBytes[OutCounter]:= Ord(wUnits[i])
          else OutBytes[OutCounter]:= 0;
          OutCounter:= OutCounter + 1;
       end;
       for i:= 1 to RepCodeLen do begin
          if (sRepCodeLen >= i) And (wRepCode <> '') then OutBytes[OutCounter]:= Ord(wRepCode[i])
          else OutBytes[OutCounter]:= 0;
          OutCounter:= OutCounter + 1;
       end;
       for i:= 1 to SamplesLen do begin
          if (sSamplesLen >= i) And (wSamples <> '') then OutBytes[OutCounter]:= Ord(wSamples[i])
          else OutBytes[OutCounter]:= 0;
          OutCounter:= OutCounter + 1;
       end;
       for i:= 1 to AbsentValueLen do begin
          if (sAbsentValueLen >= i) And (wAbsentValue <> '') then OutBytes[OutCounter]:= Ord(wAbsentValue[i])
          else OutBytes[OutCounter]:= 0;
          OutCounter:= OutCounter + 1;
       end;
    end;
  end;

  function DataChannelToStr(DataChannel: TDataChannel): String;
  var wStr: String;
  begin
    wStr:= '';
    with DataChannel do
       wStr:= wStr + DLISName + ' ' + Units + ' ' + RepCode + ' ' + IntToStr(Samples) + ' ' + AbsentValue + NewLine;
    Result:= wStr;
  end;

  function DataToStr(): String;
  var len, i: Word;
      wStr: String;
  begin
    wStr:= '';
    len:= length(Data);
    for i:=1 to len do
      if Data[i-1] > 0 then wStr:= wStr + Chr(Data[i-1]);
    Result:= wStr;
  end;

  procedure CopyRecord(RecType: Char; RecordLength: Word);
  var i, StartIndex: Word;
  begin
    OutBytes[OutCounter]:= (RecordLength + 1) And $00FF;
    OutCounter:= OutCounter + 1;
    OutBytes[OutCounter]:= (RecordLength + 1) >> 8;
    OutCounter:= OutCounter + 1;
    OutBytes[OutCounter]:= Ord(RecType);
    OutCounter:= OutCounter + 1;
    StartIndex:= 0;

    for i:=StartIndex to RecordLength - 1 do begin
      OutBytes[OutCounter]:= Data[i];
      OutCounter:= OutCounter + 1;
    end;
  end;

  procedure ConvertBinDbVersion();
  var fileLen: longWord;
      RecordLength: Word;
      RecordType: Char;
      FileCheck: String4;
  begin
    if LoadSourceFile('bin_db', 100) then begin
       setLength(DataChannels, 0);
       fileLen:= length(Bytes);
       if fileLen > 1000 then FileCheck:= Chr(Bytes[2]) + Chr(Bytes[3]) + Chr(Bytes[4]) + Chr(Bytes[5]);
       if FileCheck = 'PFFV' then begin
         Bytes[8]:= Ord(IntToStr(TFFVersion)[1]);
         SetLength(OutBytes, fileLen);
         Counter:= 0;
         OutCounter:= 0;
         repeat
            RecordLength:= Bytes[Counter];
            Counter:= Counter + 1;
            RecordLength:= (RecordLength or (Bytes[Counter] shl 8)) - 1;
            Counter:= Counter + 1;
            RecordType:= Chr(Bytes[Counter]);
            Counter:= Counter + 1;
            GetData(RecordLength);
            if RecordType = 'P' then begin
               CopyRecord('P', RecordLength);
            end
            else if RecordType = 'M' then CopyRecord('M', RecordLength)
                 else if RecordType = 'F' then CopyRecord('F', RecordLength)
                      else if RecordType = 'B' then
                      CopyRecord('B', RecordLength)
                           else if RecordType = 'D' then begin
                                   ComposeDataChannel(ParseDataChannel(RecordLength));
                                end;
         until Counter >= fileLen;
         SetLength(OutBytes, OutCounter);
         SaveByteArray(OutBytes, ReplaceText(CurrentOpenedFile, ExtractFileExt(CurrentOpenedFile),'') + '_converted.bin_db');
       end
     end;
  end;

end.

