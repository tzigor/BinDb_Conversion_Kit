unit ParseBin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  Utils, UserTypes, TffObjects;

  procedure TffDataChannelsSet(TFF_Ver: Byte);
  function BinParser(): TFrameRecords;

implementation
uses Main;

function GetCurrentByte(): Byte;
var b: Byte;
begin
  if Not EndOfFile then begin
      b:= Bytes[DataOffset];
      Inc(DataOffset);
      if isEndOfFile then Exit;
      if b = $DB then begin
         b:= Bytes[DataOffset];
         if b = $DC then Result:= $C0
         else if b = $DD then Result:= $DB
              else Result:= 0;
         Inc(DataOffset);
         if isEndOfFile then Exit;
      end
      else Result:= b;
  end
  else ErrorCode:= UNEXPECTED_END_OF_FILE;
end;

procedure DoCrc(data:byte; var crc:byte);
var i:integer;
begin
  for i:= 0 to 7 do begin
    if (((data xor crc) and 1) <> 0) then crc:= ((crc xor $18) shr 1) or $80
    else crc:= (crc shr 1) and not $80;
    data:= data shr 1;
  end;
end;

function GetCurrentRecord(): TCurrentRecord; // Offset points to ADDR of current record
var CurrentRecord: TCurrentRecord;
    i: Byte;
begin
  if (Bytes[DataOffset] and $01) > 0 then begin
     CurrentRecord.Addr:= GetCurrentByte;
     CurrentRecord.Cmd:= GetCurrentByte;
  end
  else begin
    CurrentRecord.Addr:= 0;
    CurrentRecord.Cmd:= GetCurrentByte;
  end;
  CurrentRecord.N:= GetCurrentByte;
  SetLength(CurrentRecord.Data, CurrentRecord.N);
  if CurrentRecord.N > 0 then
     for i:=0 to CurrentRecord.N - 1 do CurrentRecord.Data[i]:= GetCurrentByte;
  CurrentRecord.Crc:= GetCurrentByte;
  Result:= CurrentRecord;
end;


procedure TffDataChannelsSet(Tff_Ver: Byte);
begin
  TffStructure.Init;
  TffStructure.AddChannel('TIME', '100S', 'F4', '1', Tff_Ver);

  { Cmd - 41 }
  TffStructure.AddChannel('UCmiVmi', 'V', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('UCmaVmi', 'V', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('UCmiVma', 'V', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('UCmaVma', 'V', 'U1', '1', Tff_Ver);

  TffStructure.AddChannel('UImiVmi', 'V', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('UImaVmi', 'V', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('UImiVma', 'V', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('UImaVma', 'V', 'U1', '1', Tff_Ver);

  TffStructure.AddChannel('Tmin1', 'ms', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Tmax1', 'ms', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Tmin2st', 'ms', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Tmax2st', 'ms', 'U1', '1', Tff_Ver);

  TffStructure.AddChannel('Amin1', 'mA', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Amax1', 'mA', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Amin2st', 'mA', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Amax2st', 'mA', 'U1', '1', Tff_Ver);

  TffStructure.AddChannel('Tmin2', 'ms', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Tmax2', 'ms', 'U1', '1', Tff_Ver);

  TffStructure.AddChannel('TminRet', 'ms', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('TmaxRet', 'ms', 'U1', '1', Tff_Ver);

  TffStructure.AddChannel('Amin2', 'mA', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Amax2', 'mA', 'U1', '1', Tff_Ver);

  TffStructure.AddChannel('Nerror', 'cnt', 'U1', '1', Tff_Ver);

  TffStructure.AddChannel('TRPM', 'rpm', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('MUP_Temp', 'C', 'I1', '1', Tff_Ver);
  TffStructure.AddChannel('Shock_X', 'g', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Shock_Y', 'g', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Shock_Z', 'g', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Vib_X', 'g', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('Vib_Lat', 'g', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('Th50G', 'mcs', 'I4', '1', Tff_Ver);

  { Cmd - 42 }
  TffStructure.AddChannel('CRPM', 'rpm', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Incl_Temp', 'C', 'I1', '1', Tff_Ver);
  TffStructure.AddChannel('d_GX', 'g', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('d_GY', 'g', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('d_GZ', 'g', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('d_HX', 'h', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('d_HY', 'h', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('d_HZ', 'h', 'F4', '1', Tff_Ver);

  { Cmd - 2 }
  TffStructure.AddChannel('s_GX', 'g', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('s_GY', 'g', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('s_GZ', 'g', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('s_HX', 'h', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('s_HY', 'h', 'F4', '1', Tff_Ver);
  TffStructure.AddChannel('s_HZ', 'h', 'F4', '1', Tff_Ver);

end;

function BinParser(): TFrameRecords;
var

  //FS            : TextFile;

  s             : String;
  i             : Integer;
  d             : Double;
  w             : Word;
  ui            : UInt64;
  i64           : Int64;

  TffFrames     : TTffFrames;
  CurrentRecord : TCurrentRecord;
  b             : Byte;
  DateTime,
  PrevDateTime  : TDateTime;
  I1            : ShortInt;
  U1            : Byte;
  I2            : SmallInt;
  U2            : Word;
  I4            : LongInt;
  U4            : LongWord;
  U8            : QWord;
  F4            : Single;
  F8            : Double;

  CycleNum      : LongWord;

begin
  ErrorCode:= 0;

  //AssignFile(FS, 'DebugFile.txt');
  //Rewrite(FS);

  PrevDateTime:= 0;
  TffFrames.Init;
  DataOffset:= 0;
  EndOfFile:= False;
  CycleNum:= 0;
  repeat
    b:= 0;
    while (b <> $C0) And (Not EndOfFile) do b:= GetCurrentByte;

    if b = $C0 then begin
      CurrentRecord:= GetCurrentRecord;
      if  CurrentRecord.N > 0 then begin
        if IsValidDateTime(2000 + StrToInt(IntToHex(CurrentRecord.Data[0])), StrToInt(IntToHex(CurrentRecord.Data[1])), StrToInt(IntToHex(CurrentRecord.Data[2])),
                      StrToInt(IntToHex(CurrentRecord.Data[3])), StrToInt(IntToHex(CurrentRecord.Data[4])), StrToInt(IntToHex(CurrentRecord.Data[5])), 0) then
        begin
            DateTime:= EncodeDateTime(2000 + StrToInt(IntToHex(CurrentRecord.Data[0])), StrToInt(IntToHex(CurrentRecord.Data[1])), StrToInt(IntToHex(CurrentRecord.Data[2])),
                        StrToInt(IntToHex(CurrentRecord.Data[3])), StrToInt(IntToHex(CurrentRecord.Data[4])), StrToInt(IntToHex(CurrentRecord.Data[5])), 0);
            if (PrevDateTime <> DateTime) And ((CurrentRecord.Cmd = 41) or (CurrentRecord.Cmd = 42)) then TffFrames.AddRecord(DateTime, TffStructure.GetDataChannelSize, TffStructure.GetTFFDataChannels);
            PrevDateTime:= DateTime;

            case CurrentRecord.Cmd of
              01: {$REGION ' запись при выключении питания '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]..Buf[27]: данные. }

                    {Buf[06]..Buf[08]: время в секундах работы от включения до выключения питания. }
                    i:= 0;
                    Move(CurrentRecord.Data[6], i, 3);
                    s:= #09#09#09#09 + IntToStr(i) + #09+ 'время в секундах работы от включения до выключения питания';

                    {Buf[09]..Buf[11]: время в секундах работы в данном рейсе. }
                    i:= 0;
                    Move(CurrentRecord.Data[9], i, 3);
                    s:= #09#09#09#09 + IntToStr(i) + #09+ 'время в секундах работы в данном рейсе';

                    {Buf[12]..Buf[15]: время в секундах общей работы. }
                    Move(CurrentRecord.Data[12], i, 4);
                    s:= #09#09#09#09 + IntToStr(i) + #09+ 'время в секундах общей работы';

                    {Buf[16]..Buf[18]: общее количество ударов. }
                    i:= 0;
                    Move(CurrentRecord.Data[16], i, 3);
                    s:= #09#09#09#09 + IntToStr(i) + #09#09 + 'общее количество ударов';

                    {Buf[19]..Buf[22]: оставшееся количество ампер часов силовой батареи. }
                    Move(CurrentRecord.Data[19], i, 4);
                    s:= #09#09#09#09 + IntToStr(i) + #09#09 + 'оставшееся количество ампер-часов силовой батареи';

                    {Buf[23]: номер используемого набора пакетов. }
                    i:= CurrentRecord.Data[23];
                    s:= #09#09#09#09 + IntToStr(i) + #09#09 + 'номер используемого набора пакетов';

                    {Buf[24]: тип модуляции. }
                    i:= CurrentRecord.Data[24];
                    s:= #09#09#09#09 + IntToStr(i) + #09#09 + 'тип модуляции';

                    {Buf[25]: индекс таблицы частота/скорость передачи. }
                    i:= CurrentRecord.Data[25];
                    s:= #09#09#09#09 + IntToStr(i) + #09#09 + 'индекс таблицы частота/скорость передачи';

                    {Buf[26]..Buf[27]: код завершения работы. }
                    i:= 0;
                    Move(CurrentRecord.Data[26], i, 2);
                    s:= #09#09#09#09 + IntToStr(i) + #09#09 + 'код завершения работы';

                    {Buf[28]: напряжение холостого хода аккумуляторной батареи*50. }
                    s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[28] / 50]) + #09#09 + 'напряжение холостого хода аккумуляторной батареи*50';

                    {Buf[29]: напряжение аккумуляторной батареи под нагрузкой*50. }
                    s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[29] / 50]) + #09#09 + 'напряжение аккумуляторной батареи под нагрузкой*50';
                  end;
                  {$ENDREGION}

              02: {$REGION ' запись статического замера '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]..Buf[21]: данные. }

                    {Buf[06]..Buf[07]: показания акселерометра AX. }
                    Move(CurrentRecord.Data[06], I2, 2);
                    F4:= I2 * 1.2 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('s_GX'), F4);

                    {Buf[08]..Buf[09]: показания акселерометра AY. }
                    Move(CurrentRecord.Data[08], I2, 2);
                    F4:= I2 * 1.2 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('s_GY'), F4);

                    {Buf[10]..Buf[11]: показания акселерометра AZ. }
                    Move(CurrentRecord.Data[10], I2, 2);
                    F4:= I2 * 1.2 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('s_GZ'), F4);

                    {Buf[12]..Buf[13]: показания магнитометра BX. }
                    Move(CurrentRecord.Data[12], I2, 2);
                    F4:= I2 * 120000 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('s_HX'), F4);

                    {Buf[14]..Buf[15]: показания магнитометра BY. }
                    Move(CurrentRecord.Data[14], I2, 2);
                    F4:= I2 * 120000 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('s_HY'), F4);

                    {Buf[16]..Buf[17]: показания магнитометра BZ. }
                    Move(CurrentRecord.Data[16], I2, 2);
                    F4:= I2 * 120000 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('s_HZ'), F4);

                    {Buf[18]..Buf[19]: приращение отклонителя. }
                    Move(CurrentRecord.Data[18], I2, 2);
                    s:= #09#09#09#09 + IntToStr(I2) + #09#09 + 'приращение отклонителя';

                    {Buf[20]..Buf[21]: максимум пульсаций акселерометров. }
                    Move(CurrentRecord.Data[20], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'максимум пульсаций акселерометров';
                  end;
                  {$ENDREGION}

              03: {$REGION ' запись динамического замера '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]..Buf[19]: данные. }

                    {Buf[06]..Buf[07]: показания акселерометра AX. }
                    Move(CurrentRecord.Data[06], I2, 2);
                    F4:= I2 * 1.2 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('d_GX'), F4);

                    {Buf[08]..Buf[09]: показания акселерометра AY. }
                    Move(CurrentRecord.Data[08], I2, 2);
                    F4:= I2 * 1.2 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('d_GY'), F4);

                    {Buf[10]..Buf[11]: показания акселерометра AZ. }
                    Move(CurrentRecord.Data[10], I2, 2);
                    F4:= I2 * 1.2 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('d_GZ'), F4);

                    {Buf[12]..Buf[13]: показания магнитометра BX. }
                    Move(CurrentRecord.Data[12], I2, 2);
                    F4:= I2 * 120000 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('d_HX'), F4);

                    {Buf[14]..Buf[15]: показания магнитометра BY. }
                    Move(CurrentRecord.Data[14], I2, 2);
                    F4:= I2 * 120000 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('d_HY'), F4);

                    {Buf[16]..Buf[17]: показания магнитометра BZ. }
                    Move(CurrentRecord.Data[16], I2, 2);
                    F4:= I2 * 120000 / $7FFF;
                    TffFrames.AddData(TffStructure.GetOffsetByName('d_HZ'), F4);

                    {Buf[18]: скорость буровой колонны об/мин. }
                    U1:= CurrentRecord.Data[18];
                    TffFrames.AddData(TffStructure.GetOffsetByName('CRPM'), U1);

                    {Buf[19]: температура модуля инклинометра. }
                    I1:= CurrentRecord.Data[19];
                    TffFrames.AddData(TffStructure.GetOffsetByName('Incl_Temp'), I1);
                  end;
                  {$ENDREGION}

              04: {$REGION ' запись смещения отклонителя '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]..Buf[07]: данные. }

                    {Buf[06]..Buf[07]: смещение отклонителя. }
                    Move(CurrentRecord.Data[6], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'смещение отклонителя';
                  end;
                  {$ENDREGION}

              06: {$REGION ' запись параметров передачи силового модуля '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]..Buf[26]: данные. }

                    {Buf[06]..Buf[07]: ток инвертора. }
                    Move(CurrentRecord.Data[6], w, 2);
                    s:= #09#09#09#09 + Format('%.1f', [w / 10]) + #09#09 + 'ток инвертора';

                    {Buf[08]..Buf[09]: максимум входного напряжения. }
                    Move(CurrentRecord.Data[8], w, 2);
                    s:= #09#09#09#09 + Format('%.1f', [w / 10]) + #09#09 + 'максимум входного напряжения';

                    {Buf[10]..Buf[11]: минимум входного напряжения. }
                    Move(CurrentRecord.Data[10], w, 2);
                    s:= #09#09#09#09 + Format('%.1f', [w / 10]) + #09#09 + 'минимум входного напряжения';

                    {Buf[12]..Buf[13]: обороты генератора. }
                    Move(CurrentRecord.Data[12], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'обороты генератора';

                    {Buf[14]..Buf[15]: сопротивление нагрузки. }
                    Move(CurrentRecord.Data[14], w, 2);
                    s:= #09#09#09#09 + Format('%.3f', [w / 1000]) + #09#09 + 'сопротивление нагрузки';

                    {Buf[16]..Buf[17]: приращение ампер-часов силовой батареи. }
                    Move(CurrentRecord.Data[16], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'приращение ампер-часов силовой батареи';

                    {Buf[18]..Buf[19]: количество ударов. }
                    Move(CurrentRecord.Data[18], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'количество ударов';

                    {Buf[20]..Buf[21]: максимум по оси Х. }
                    Move(CurrentRecord.Data[20], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'максимум по оси Х';

                    {Buf[22]..Buf[23]: максимум по оси Y. }
                    Move(CurrentRecord.Data[22], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'максимум по оси Y';

                    {Buf[24]..Buf[25]: максимум по оси Z. }
                    Move(CurrentRecord.Data[24], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'максимум по оси Z';

                    {Buf[26]: максимум количества ударов в секунду. }
                    s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[26]) + #09#09 + 'максимум количества ударов в секунду';

                    {Buf[27]: степень залипания колонны. }
                    Move(CurrentRecord.Data[27], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'степень залипания колонны';
                  end;
                  {$ENDREGION}


              24: {$REGION ' запись тока статики '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]..Buf[17]: данные. }

                    {Buf[06]..Buf[07]: ток статики при частоте 10 Гц. }
                    Move(CurrentRecord.Data[06], w, 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 10 Гц, А';

                    {Buf[08]..Buf[09]: ток статики при частоте 5 Гц. }
                    Move(CurrentRecord.Data[08], w, 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 5 Гц, А';

                    {Buf[10]..Buf[11]: ток статики при частоте 2.5 Гц. }
                    Move(CurrentRecord.Data[10], w, 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 2.5 Гц, А';

                    {Buf[12]..Buf[13]: ток статики при частоте 1.25 Гц. }
                    Move(CurrentRecord.Data[12], w, 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 1.25 Гц, А';

                    {Buf[14]..Buf[15]: ток статики при частоте 0.625 Гц. }
                    Move(CurrentRecord.Data[14], w, 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 0.625 Гц, А';

                    {Buf[16]..Buf[17]: ток статики при частоте 0.3125 Гц. }
                    Move(CurrentRecord.Data[16], w, 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 0.3125 Гц, А';
                  end;
                  {$ENDREGION}

              25: {$REGION ' запись тока динамики '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]..Buf[17]: данные. }

                    {Buf[06]..Buf[07]: ток динамики при частоте 10 Гц. }
                    Move(CurrentRecord.Data[06], w, 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 10 Гц, А';

                    {Buf[08]..Buf[09]: ток динамики при частоте 5 Гц. }
                    Move(CurrentRecord.Data[08], w, 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 5 Гц, А';

                    {Buf[10]..Buf[11]: ток динамики при частоте 2.5 Гц. }
                    Move(CurrentRecord.Data[10], w, 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 2.5 Гц, А';

                    {Buf[12]..Buf[13]: ток динамики при частоте 1.25 Гц. }
                    Move(CurrentRecord.Data[12], w, 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 1.25 Гц, А';

                    {Buf[14]..Buf[15]: ток динамики при частоте 0.625 Гц. }
                    Move(CurrentRecord.Data[14], w , 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 0.625 Гц, А';

                    {Buf[16]..Buf[17]: ток динамики при частоте 0.3125 Гц. }
                    Move(CurrentRecord.Data[16], w , 2);
                    d:= w / 10;
                    s:= #09#09#09#09 + FormatFloat('#0.0', d) + #09#09 + 'при частоте 0.3125 Гц, А';
                  end;
                  {$ENDREGION}

              26: {$REGION ' запись интервалов времени манипуляции давлением '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]..Buf[13]: данные. }

                    {Buf[06]..Buf[07]: интервал времени Т0. }
                    Move(CurrentRecord.Data[06], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'интервал времени Т0, с';

                    {Buf[08]..Buf[09]: интервал времени Т1. }
                    Move(CurrentRecord.Data[08], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'интервал времени Т1, с';

                    {Buf[10]..Buf[11]: интервал времени Т2. }
                    Move(CurrentRecord.Data[10], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'интервал времени Т2, с';

                    {Buf[12]..Buf[13]: интервал времени Т3. }
                    Move(CurrentRecord.Data[12], w, 2);
                    s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'интервал времени Т3, с';
                  end;
                  {$ENDREGION}

              37: {$REGION ' обороты колонны*100 [об/мин], выше которых включаются роторные пакеты '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]..Buf[07]: данные. }

                    {Buf[06]..Buf[07]: 'Верхний порог оборотов колонны' }
                    Move(CurrentRecord.Data[06], w, 2);
                    d:= w / 100;
                    s:= #09#09#09#09 + FormatFloat('#0.00', d) + ' об/мин';
                  end;
                  {$ENDREGION}

              40: {$REGION ' запись при выключении питания - другой формат ударов (бывш. 01) '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]         : № версии для записи 40.
                     Buf[07]..Buf[50]: данные.                 }

                    {далее интерпретируем содержимое записи в зав. от номера её версии. }
                    case CurrentRecord.Data[6] of                                 {в новом формате записей в байте [6] всегда лежит № версии                      }
                      00: {$REGION ' версия №00 '}
                          begin
                            {Buf[07]..Buf[09]: время в секундах работы от включения до выключения питания. }
                            i:= 0;
                            Move(CurrentRecord.Data[07], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы от включения до выключения питания';

                            {Buf[10]..Buf[12]: время в секундах работы в данном рейсе. }
                            i:= 0;
                            Move(CurrentRecord.Data[10], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы в данном рейсе';

                            {Buf[13]..Buf[16]: время в секундах общей работы. }
                            Move(CurrentRecord.Data[13], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время общей работы';

                            {Buf[17]: средняя величина удара превышающего 50G (в G) по X. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[17]) + #09+ 'средняя величина удара превышающего 50G (в G) по X';

                            {Buf[18]..Buf[25]: время длит. среднего удара превышающего 50G (*25мкс) по X. }
                            Move(CurrentRecord.Data[18], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс) по X';

                            {Buf[26]: средняя величина удара превышающего 50G (в G) по Y. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[26]) + #09+ 'средняя величина удара превышающего 50G (в G) по Y';

                            {Buf[27]..Buf[34]: время длит. среднего удара превышающего 50G (*25мкс) по Y. }
                            Move(CurrentRecord.Data[27], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс) по Y';

                            {Buf[35]: средняя величина удара превышающего 50G (в G) по Z. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[35]) + #09+ 'средняя величина удара превышающего 50G (в G) по Z';

                            {Buf[36]..Buf[43]: время длит. среднего удара превышающего 50G (*25мкс) по Z. }
                            Move(CurrentRecord.Data[36], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс) по Z';

                            {Buf[44]: номер используемого набора пакетов. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[44]) + #09+ 'номер используемого набора пакетов';

                            {Buf[46]: индекс таблицы частота/скорость передачи. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[46]) + #09+ 'индекс таблицы частота/скорость передачи';

                            {Buf[47]..Buf[48]: код завершения работы. }
                            s:= #09#09#09#09 + 'код завершения работы';
                            //CurrentRecord.Data[47];
                            //CurrentRecord.Data[48];

                            {Buf[49]: напряжение холостого хода аккумуляторной батареи*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[49] / 50]) + #09+ 'напряжение холостого хода аккумуляторной батареи*50';

                            {Buf[50]: напряжение аккумуляторной батареи под нагрузкой*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[50] / 50]) + #09+ 'напряжение аккумуляторной батареи под нагрузкой*50';
                          end;
                          {$ENDREGION}

                      01: {$REGION ' версия №01 '}
                          begin
                            {Buf[07]..Buf[09]: время в секундах работы от включения до выключения питания. }
                            i:= 0;
                            Move(CurrentRecord.Data[07], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы от включения до выключения питания';

                            {Buf[10]..Buf[12]: время в секундах работы в данном рейсе. }
                            i:= 0;
                            Move(CurrentRecord.Data[10], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы в данном рейсе';

                            {Buf[13]..Buf[16]: время в секундах общей работы. }
                            Move(CurrentRecord.Data[13], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время общей работы';

                            {Buf[17]: средняя величина удара превышающего 50G (в G) по X. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[17]) + #09+ 'средняя величина удара превышающего 50G (в G) по X';

                            {Buf[18]..Buf[25]: время длит. среднего удара превышающего 50G (*25мкс) по X. }
                            Move(CurrentRecord.Data[18], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс) по X';

                            {Buf[26]: средняя величина удара превышающего 50G (в G) по Y. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[26]) + #09+ 'средняя величина удара превышающего 50G (в G) по Y';

                            {Buf[27]..Buf[34]: время длит. среднего удара превышающего 50G (*25мкс) по Y. }
                            Move(CurrentRecord.Data[27], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс) по Y';

                            {Buf[35]: средняя величина удара превышающего 50G (в G) по Z. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[35]) + #09+ 'средняя величина удара превышающего 50G (в G) по Z';

                            {Buf[36]..Buf[43]: время длит. среднего удара превышающего 50G (*25мкс) по Z. }
                            Move(CurrentRecord.Data[36], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс) по Z';

                            {Buf[44]: номер используемого набора пакетов. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[44]) + #09+ 'номер используемого набора пакетов';

                            {Buf[46]: индекс таблицы частота/скорость передачи. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[46]) + #09+ 'индекс таблицы частота/скорость передачи';

                            {Buf[47]..Buf[48]: код завершения работы. }
                            s:= #09#09#09#09 + 'код завершения работы';
                            //CurrentRecord.Data[28];
                            //CurrentRecord.Data[29];

                            {Buf[49]: напряжение холостого хода аккумуляторной батареи*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[49] / 50]) + #09+ 'напряжение холостого хода аккумуляторной батареи*50';

                            {Buf[50]: напряжение аккумуляторной батареи под нагрузкой*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[50] / 50]) + #09+ 'напряжение аккумуляторной батареи под нагрузкой*50';

                            {Buf[51]..Buf[52]: ограничение мощности [Вт]. }
                            Move(CurrentRecord.Data[51], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + ' Вт'#09+ 'ограничение мощности';
                          end;
                          {$ENDREGION}

                      02: {$REGION ' версия №02 '}
                          begin
                            {Buf[07]..Buf[09]: время в секундах работы от включения до выключения питания. }
                            i:= 0;
                            Move(CurrentRecord.Data[07], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы от включения до выключения питания';

                            {Buf[10]..Buf[12]: время в секундах работы в данном рейсе. }
                            i:= 0;
                            Move(CurrentRecord.Data[10], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы в данном рейсе';

                            {Buf[13]..Buf[16]: время в секундах общей работы. }
                            Move(CurrentRecord.Data[13], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время общей работы';

                            {Buf[17]: средняя величина удара превышающего 50G (в G) по X. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[17]) + #09+ 'средняя величина удара превышающего 50G (в G) по X';

                            {Buf[18]..Buf[25]: время длит. среднего удара превышающего 50G (*25мкс) по X. }
                            Move(CurrentRecord.Data[18], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс) по X';

                            {Buf[26]: средняя величина удара превышающего 50G (в G) по Y. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[26]) + #09+ 'средняя величина удара превышающего 50G (в G) по Y';

                            {Buf[27]..Buf[34]: время длит. среднего удара превышающего 50G (*25мкс) по Y. }
                            Move(CurrentRecord.Data[27], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс) по Y';

                            {Buf[35]: средняя величина удара превышающего 50G (в G) по Z. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[35]) + #09+ 'средняя величина удара превышающего 50G (в G) по Z';

                            {Buf[36]..Buf[43]: время длит. среднего удара превышающего 50G (*25мкс) по Z. }
                            Move(CurrentRecord.Data[36], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс) по Z';

                            {Buf[44]: номер используемого набора пакетов. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[44]) + #09+ 'номер используемого набора пакетов';

                            {Buf[46]: индекс таблицы частота/скорость передачи. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[46]) + #09+ 'индекс таблицы частота/скорость передачи';

                            {Buf[47]..Buf[48]: код завершения работы. }
                            s:= #09#09#09#09 + 'код завершения работы';
                            //CurrentRecord.Data[28];
                            //CurrentRecord.Data[29];

                            {Buf[49]: напряжение холостого хода аккумуляторной батареи*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[49] / 50]) + #09+ 'напряжение холостого хода аккумуляторной батареи*50';

                            {Buf[50]: напряжение аккумуляторной батареи под нагрузкой*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[50] / 50]) + #09+ 'напряжение аккумуляторной батареи под нагрузкой*50';

                            {Buf[51]..Buf[52]: ограничение мощности [Вт]. }
                            Move(CurrentRecord.Data[51], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + ' Вт'#09+ 'ограничение мощности';

                            {Buf[53]..Buf[55]: энергия цикла заряда аккумулятора [мА*мин]. }
                            i:= 0;
                            Move(CurrentRecord.Data[53], i, 3);
                            d:= i / 496200;                                       {переводим из мА*мин в А*час                                                    }
                            s:= #09#09#09#09 + FormatFloat('#0.000000', d) + #09 + 'энергия цикла заряда аккумулятора, А*час';
                          end;
                          {$ENDREGION}

                      03: {$REGION ' версия №03 '}
                          begin
                            {Buf[07]..Buf[09]: время в секундах работы от включения до выключения питания. }
                            i:= 0;
                            Move(CurrentRecord.Data[07], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы от включения до выключения питания';

                            {Buf[10]..Buf[12]: время в секундах работы в данном рейсе. }
                            i:= 0;
                            Move(CurrentRecord.Data[10], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы в данном рейсе';

                            {Buf[13]..Buf[16]: время в секундах общей работы. }
                            Move(CurrentRecord.Data[13], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время общей работы';

                            {Buf[17]..Buf[24]: время длит. среднего удара превышающего 50G (*25мкс) по X. }
                            Move(CurrentRecord.Data[17], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс)';

                            {Buf[25]: номер используемого набора пакетов. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[25]) + #09+ 'номер используемого набора пакетов';

                            {Buf[27]: индекс таблицы частота/скорость передачи. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[27]) + #09+ 'индекс таблицы частота/скорость передачи';

                            {Buf[28]..Buf[29]: код завершения работы. }
                            s:= #09#09#09#09 + 'код завершения работы';
                            //CurrentRecord.Data[28];
                            //CurrentRecord.Data[29];

                            {Buf[30]: напряжение холостого хода аккумуляторной батареи*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[30] / 50]) + #09+ 'напряжение холостого хода аккумуляторной батареи*50';

                            {Buf[31]: напряжение аккумуляторной батареи под нагрузкой*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[31] / 50]) + #09+ 'напряжение аккумуляторной батареи под нагрузкой*50';

                            {Buf[32]..Buf[33]: ограничение мощности [Вт]. }
                            Move(CurrentRecord.Data[32], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + ' Вт'#09+ 'ограничение мощности';

                            {Buf[34]..Buf[36]: энергия цикла заряда аккумулятора [мА*мин]. }
                            i:= 0;
                            Move(CurrentRecord.Data[34], i, 3);
                            d:= i / 496200;                                       {переводим из мА*мин в А*час                                                    }
                            s:= #09#09#09#09 + FormatFloat('#0.000000', d) + #09 + 'энергия цикла заряда аккумулятора, А*час';
                          end;
                          {$ENDREGION}

                      04: {$REGION ' версия №04 '}
                          begin
                            {Buf[07]..Buf[09]: время в секундах работы от включения до выключения питания. }
                            i:= 0;
                            Move(CurrentRecord.Data[07], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы от включения до выключения питания';

                            {Buf[10]..Buf[12]: время в секундах работы в данном рейсе. }
                            i:= 0;
                            Move(CurrentRecord.Data[10], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы в данном рейсе';

                            {Buf[13]..Buf[16]: время в секундах общей работы. }
                            Move(CurrentRecord.Data[13], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время общей работы';

                            {Buf[17]..Buf[24]: время длит. среднего удара превышающего 50G (*25мкс) по X. }
                            Move(CurrentRecord.Data[17], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс)';

                            {Buf[25]: номер используемого набора пакетов. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[25]) + #09+ 'номер используемого набора пакетов';

                            {Buf[27]: индекс таблицы частота/скорость передачи. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[27]) + #09+ 'индекс таблицы частота/скорость передачи';

                            {Buf[28]..Buf[29]: код завершения работы. }
                            s:= #09#09#09#09 + 'код завершения работы';
                            //CurrentRecord.Data[28];
                            //CurrentRecord.Data[29];

                            {Buf[30]: напряжение холостого хода аккумуляторной батареи*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[30] / 50]) + #09+ 'напряжение холостого хода аккумуляторной батареи*50';

                            {Buf[31]: напряжение аккумуляторной батареи под нагрузкой*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[31] / 50]) + #09+ 'напряжение аккумуляторной батареи под нагрузкой*50';

                            {Buf[32]..Buf[33]: ограничение мощности [Вт]. }
                            Move(CurrentRecord.Data[32], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + ' Вт'#09+ 'ограничение мощности';

                            {Buf[34]..Buf[36]: энергия цикла заряда аккумулятора [мА*мин]. }
                            i:= 0;
                            Move(CurrentRecord.Data[34], i, 3);
                            d:= i / 496200;                                       {переводим из мА*мин в А*час                                                    }
                            s:= #09#09#09#09 + FormatFloat('#0.000000', d) + #09 + 'энергия цикла заряда аккумулятора, А*час';

                          end;
                          {$ENDREGION}

                      05:  {$REGION ' версия №05 '}
                          begin
                            {Buf[07]..Buf[09]: время в секундах работы от включения до выключения питания. }
                            i:= 0;
                            Move(CurrentRecord.Data[07], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы от включения до выключения питания';

                            {Buf[10]..Buf[12]: время в секундах работы в данном рейсе. }
                            i:= 0;
                            Move(CurrentRecord.Data[10], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы в данном рейсе';

                            {Buf[13]..Buf[16]: время в секундах общей работы. }
                            Move(CurrentRecord.Data[13], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время общей работы';

                            {Buf[17]..Buf[24]: время длит. среднего удара превышающего 50G (*25мкс) по X. }
                            Move(CurrentRecord.Data[17], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс)';

                            {Buf[25]: номер используемого набора пакетов. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[25]) + #09+ 'номер используемого набора пакетов';

                            {Buf[27]: индекс таблицы частота/скорость передачи. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[27]) + #09+ 'индекс таблицы частота/скорость передачи';


                            {Buf[28]..Buf[29]: код завершения работы. }
                            s:= #09#09#09#09 + 'код завершения работы';
                            //CurrentRecord.Data[28];
                            //CurrentRecord.Data[29];

                            {Buf[30]: напряжение холостого хода аккумуляторной батареи*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[30] / 50]) + #09+ 'напряжение холостого хода аккумуляторной батареи*50';

                            {Buf[31]: напряжение аккумуляторной батареи под нагрузкой*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[31] / 50]) + #09+ 'напряжение аккумуляторной батареи под нагрузкой*50';

                            {Buf[32]..Buf[33]: ограничение мощности [Вт]. }
                            Move(CurrentRecord.Data[32], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + ' Вт'#09+ 'ограничение мощности';

                            {Buf[34]..Buf[36]: энергия цикла заряда аккумулятора [мА*мин]. }
                            i:= 0;
                            Move(CurrentRecord.Data[34], i, 3);
                            d:= i / 496200;                                       {переводим из мА*мин в А*час                                                    }
                            s:= #09#09#09#09 + FormatFloat('#0.000000', d) + #09 + 'энергия цикла заряда аккумулятора, А*час';

                          end;
                          {$ENDREGION}

                      06:  {$REGION ' версия №06 '}
                          begin
                            {Buf[07]..Buf[09]: время в секундах работы от включения до выключения питания. }
                            i:= 0;
                            Move(CurrentRecord.Data[07], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы от включения до выключения питания';

                            {Buf[10]..Buf[12]: время в секундах работы в данном рейсе. }
                            i:= 0;
                            Move(CurrentRecord.Data[10], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы в данном рейсе';

                            {Buf[13]..Buf[16]: время в секундах общей работы. }
                            Move(CurrentRecord.Data[13], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время общей работы';

                            {Buf[17]..Buf[24]: время длит. среднего удара превышающего 50G (*25мкс) по X. }
                            Move(CurrentRecord.Data[17], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс)';

                            {Buf[25]: номер используемого набора пакетов. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[25]) + #09+ 'номер используемого набора пакетов';

                            {Buf[27]: индекс таблицы частота/скорость передачи. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[27]) + #09+ 'индекс таблицы частота/скорость передачи';

                            {Buf[28]..Buf[29]: код завершения работы. }
                            s:= #09#09#09#09 + 'код завершения работы';
                            //CurrentRecord.Data[28];
                            //CurrentRecord.Data[29];

                            {Buf[30]: напряжение холостого хода аккумуляторной батареи*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[30] / 50]) + #09+ 'напряжение холостого хода аккумуляторной батареи*50';

                            {Buf[31]: напряжение аккумуляторной батареи под нагрузкой*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[31] / 50]) + #09+ 'напряжение аккумуляторной батареи под нагрузкой*50';

                            {Buf[32]..Buf[33]: ограничение мощности [Вт]. }
                            Move(CurrentRecord.Data[32], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + ' Вт'#09+ 'ограничение мощности';

                            {Buf[34]..Buf[36]: энергия цикла заряда аккумулятора [мА*мин]. }
                            i:= 0;
                            Move(CurrentRecord.Data[34], i, 3);
                            d:= i / 496200;                                       {переводим из мА*мин в А*час                                                    }
                            s:= #09#09#09#09 + FormatFloat('#0.000000', d) + #09 + 'энергия цикла заряда аккумулятора, А*час';

                             {Buf[39]..Buf[46]: время длит. сред. удара превышающего 100G (*25мкс)  }
                            Move(CurrentRecord.Data[39], i64, 8);
                            s:= #09#09#09#09 + IntToStr(i64) + #09+ 'время длит. сред. удара превышающего 100G (*25мкс)';

                            {Buf[47]..Buf[54]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[47], i64, 8);
                            s:= #09#09#09#09 + IntToStr(i64) + #09+ 'время длит. сред. удара превышающего 150G (*25мкс)';

                          end;
                          {$ENDREGION}

                      07:  {$REGION ' версия №07 '}
                          begin
                            {Buf[07]..Buf[09]: время в секундах работы от включения до выключения питания. }
                            i:= 0;
                            Move(CurrentRecord.Data[07], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы от включения до выключения питания';

                            {Buf[10]..Buf[12]: время в секундах работы в данном рейсе. }
                            i:= 0;
                            Move(CurrentRecord.Data[10], i, 3);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время работы в данном рейсе';

                            {Buf[13]..Buf[16]: время в секундах общей работы. }
                            Move(CurrentRecord.Data[13], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время общей работы';

                            {Buf[17]..Buf[24]: время длит. среднего удара превышающего 50G (*25мкс) по X. }
                            Move(CurrentRecord.Data[17], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс)';

                            {Buf[25]: номер используемого набора пакетов. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[25]) + #09+ 'номер используемого набора пакетов';

                            {Buf[27]: индекс таблицы частота/скорость передачи. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[27]) + #09+ 'индекс таблицы частота/скорость передачи';

                            {Buf[28]..Buf[29]: код завершения работы. }
                            s:= #09#09#09#09 + 'код завершения работы';
                            //CurrentRecord.Data[28];
                            //CurrentRecord.Data[29];

                            {Buf[30]: напряжение холостого хода аккумуляторной батареи*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[30] / 50]) + #09+ 'напряжение холостого хода аккумуляторной батареи*50';

                            {Buf[31]: напряжение аккумуляторной батареи под нагрузкой*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[31] / 50]) + #09+ 'напряжение аккумуляторной батареи под нагрузкой*50';

                            {Buf[32]..Buf[33]: ограничение мощности [Вт]. }
                            Move(CurrentRecord.Data[32], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + ' Вт'#09+ 'ограничение мощности';

                            {Buf[34]..Buf[36]: энергия цикла заряда аккумулятора [мА*мин]. }
                            i:= 0;
                            Move(CurrentRecord.Data[34], i, 3);
                            d:= i / 496200;                                       {переводим из мА*мин в А*час                                                    }
                            s:= #09#09#09#09 + FormatFloat('#0.000000', d) + #09 + 'энергия цикла заряда аккумулятора, А*час';

                             {Buf[39]..Buf[46]: время длит. сред. удара превышающего 100G (*25мкс)  }
                            Move(CurrentRecord.Data[39], i64, 8);
                            s:= #09#09#09#09 + IntToStr(i64) + #09+ 'время длит. сред. удара превышающего 100G (*25мкс)';

                            {Buf[47]..Buf[54]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[47], i64, 8);
                            s:= #09#09#09#09 + IntToStr(i64) + #09+ 'время длит. сред. удара превышающего 150G (*25мкс)';

                            {Buf[55]: темепратура  }
                            Move(CurrentRecord.Data[55], I1, 1);
                            s:= #09#09#09#09 + IntToStr(I1) + #09+ 'температура инклинометра';

                          end;
                          {$ENDREGION}

                      08:  {$REGION ' версия №08 '}
                          begin
                            {Buf[07]..Buf[09]: время в секундах работы от включения до выключения питания. }
                            i:= 0;
                            Move(CurrentRecord.Data[07], i, 3);
                            s:= #09#09#09#09 + FormatSeconds(i) + #09+ 'время работы от включения до выключения питания';

                            {Buf[10]..Buf[12]: время в секундах работы в данном рейсе. }
                            i:= 0;
                            Move(CurrentRecord.Data[10], i, 3);
                            s:= #09#09#09#09 + FormatSeconds(i) + #09+ 'время работы в данном рейсе';

                            {Buf[13]..Buf[16]: время в секундах общей работы. }
                            Move(CurrentRecord.Data[13], i, 4);
                            s:= #09#09#09#09 + FormatSeconds(i) + #09+ 'время общей работы';

                            {Buf[17]..Buf[24]: время длит. среднего удара превышающего 50G (*25мкс) по X. }
                            Move(CurrentRecord.Data[17], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс)';

                            {Buf[25]: номер используемого набора пакетов. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[25]) + #09+ 'номер используемого набора пакетов';

                            {Buf[27]: индекс таблицы частота/скорость передачи. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[27]) + #09+ 'индекс таблицы частота/скорость передачи';

                            {Buf[28]..Buf[29]: код завершения работы. }
                            s:= #09#09#09#09 + 'код завершения работы';
                            //CurrentRecord.Data[28];
                            //CurrentRecord.Data[29];

                            {Buf[30]: напряжение холостого хода аккумуляторной батареи*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[30] / 50]) + #09+ 'напряжение холостого хода аккумуляторной батареи*50';

                            {Buf[31]: напряжение аккумуляторной батареи под нагрузкой*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[31] / 50]) + #09+ 'напряжение аккумуляторной батареи под нагрузкой*50';

                            {Buf[32]..Buf[33]: ограничение мощности [Вт]. }
                            Move(CurrentRecord.Data[32], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + ' Вт'#09+ 'ограничение мощности';

                            {Buf[34]..Buf[36]: энергия цикла заряда аккумулятора [мА*мин]. }
                            i:= 0;
                            Move(CurrentRecord.Data[34], i, 3);
                            d:= i / 496200;                                       {переводим из мА*мин в А*час                                                    }
                            s:= #09#09#09#09 + FormatFloat('#0.000000', d) + #09 + 'энергия цикла заряда аккумулятора, А*час';

                             {Buf[39]..Buf[46]: время длит. сред. удара превышающего 100G (*25мкс)  }
                            Move(CurrentRecord.Data[39], i64, 8);
                            s:= #09#09#09#09 + IntToStr(i64) + #09+ 'время длит. сред. удара превышающего 100G (*25мкс)';

                            {Buf[47]..Buf[54]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[47], i64, 8);
                            s:= #09#09#09#09 + IntToStr(i64) + #09+ 'время длит. сред. удара превышающего 150G (*25мкс)';

                            {Buf[55]: темепратура  }
                            Move(CurrentRecord.Data[55], I1, 1);
                            s:= #09#09#09#09 + IntToStr(I1) + #09+ 'температура инклинометра';

                             {Buf[56-57]: оставшиеся время в минутах до включения передачи }
                            Move(CurrentRecord.Data[56], w, 2);
                            s:= #09#09#09#09  + IntToStr(w)  + ' мин.' + #09 + 'оставшиеся время до включения передачи';

                          end;
                          {$ENDREGION}

                      09:  {$REGION ' версия №09 '}
                          begin
                            {Buf[07]..Buf[09]: время в секундах работы от включения до выключения питания. }
                            i:= 0;
                            Move(CurrentRecord.Data[07], i, 3);
                            s:= #09#09#09#09 + FormatSeconds(i) + #09+ 'время работы от включения до выключения питания';

                            {Buf[10]..Buf[12]: время в секундах работы в данном рейсе. }
                            i:= 0;
                            Move(CurrentRecord.Data[10], i, 3);
                            s:= #09#09#09#09 + FormatSeconds(i) + #09+ 'время работы в данном рейсе';

                            {Buf[13]..Buf[16]: время в секундах общей работы. }
                            Move(CurrentRecord.Data[13], i, 4);
                            s:= #09#09#09#09 + FormatSeconds(i) + #09+ 'время общей работы';

                            {Buf[17]..Buf[24]: время длит. среднего удара превышающего 50G (*25мкс) по X. }
                            Move(CurrentRecord.Data[17], ui, 8);
                            s:= #09#09#09#09 + IntToStr(ui) + #09+ 'время длит. среднего удара превышающего 50G (*25мкс)';

                            {Buf[25]: номер используемого набора пакетов. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[25]) + #09+ 'номер используемого набора пакетов';

                            {Buf[27]: индекс таблицы частота/скорость передачи. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[27]) + #09+ 'индекс таблицы частота/скорость передачи';

                            {Buf[28]..Buf[29]: код завершения работы. }
                            s:= #09#09#09#09 + 'код завершения работы';
                            //CurrentRecord.Data[28];
                            //CurrentRecord.Data[29]

                            {Buf[30]: напряжение холостого хода аккумуляторной батареи*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[30] / 50]) + #09+ 'напряжение холостого хода аккумуляторной батареи*50';

                            {Buf[31]: напряжение аккумуляторной батареи под нагрузкой*50. }
                            s:= #09#09#09#09 + Format('%.2f', [CurrentRecord.Data[31] / 50]) + #09+ 'напряжение аккумуляторной батареи под нагрузкой*50';

                            {Buf[32]..Buf[33]: ограничение мощности [Вт]. }
                            Move(CurrentRecord.Data[32], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + ' Вт'#09+ 'ограничение мощности';

                            {Buf[34]..Buf[36]: энергия цикла заряда аккумулятора [мА*мин]. }
                            i:= 0;
                            Move(CurrentRecord.Data[34], i, 3);
                            d:= i / 496200;                                       {переводим из мА*мин в А*час                                                    }
                            s:= #09#09#09#09 + FormatFloat('#0.000000', d) + #09 + 'энергия цикла заряда аккумулятора, А*час';

                             {Buf[39]..Buf[46]: время длит. сред. удара превышающего 100G (*25мкс)  }
                            Move(CurrentRecord.Data[39], i64, 8);
                            s:= #09#09#09#09 + IntToStr(i64) + #09+ 'время длит. сред. удара превышающего 100G (*25мкс)';

                            {Buf[47]..Buf[54]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[47], i64, 8);
                            s:= #09#09#09#09 + IntToStr(i64) + #09+ 'время длит. сред. удара превышающего 150G (*25мкс)';


                            {Buf[55]: темепратура  }
                            Move(CurrentRecord.Data[55], I1, 1);
                            s:= #09#09#09#09 + IntToStr(I1) + #09+ 'температура инклинометра';

                               {Buf[56-57]: оставшиеся время в минутах до включения передачи }
                            Move(CurrentRecord.Data[56], w, 2);
                            s:= #09#09#09#09  + IntToStr(w)  + ' мин.' + #09 + 'оставшиеся время до включения передачи';

                             {Buf[58-59]: нуль датчика ударов по оси Х }
                            Move(CurrentRecord.Data[58], w, 2);
                            s:= #09#09#09#09  + IntToStr(w)  +  #09 + 'нуль датчика ударов по оси Х';

                             {Buf[60-61]: нуль датчика ударов по оси Y }
                            Move(CurrentRecord.Data[60], w, 2);
                            s:= #09#09#09#09  + IntToStr(w)  +  #09 + 'нуль датчика ударов по оси Y';

                            {Buf[62-63]: нуль датчика ударов по оси Z }
                            Move(CurrentRecord.Data[62], w, 2);
                            s:= #09#09#09#09  + IntToStr(w)  +  #09 + 'нуль датчика ударов по оси Z';

                          end;
                          {$ENDREGION}

                    end;
                  end;
                  {$ENDREGION}

              41: {$REGION ' запись параметров передачи силового модуля - новый формат ударов (бывш. 06) '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]         : № версии для записи 41.
                     Buf[07]..Buf[35]: данные.                 }

                    case CurrentRecord.Data[6] of
                      05: {$REGION ' версия №05 '}
                          begin
                            {Buf[07]..Buf[08]: ток инвертора. }
                            Move(CurrentRecord.Data[07], w, 2);
                            s:= #09#09#09#09 + Format('%.1f', [w / 10]) + #09+ 'ток инвертора';

                            {Buf[09]..Buf[10]: максимум входного напряжения. }
                            Move(CurrentRecord.Data[09], w, 2);
                            s:= #09#09#09#09 + Format('%.1f', [w / 10]) + #09+ 'максимум входного напряжения';

                            {Buf[11]..Buf[12]: минимум входного напряжения. }
                            Move(CurrentRecord.Data[11], w, 2);
                            s:= #09#09#09#09 + Format('%.1f', [w / 10]) + #09+ 'минимум входного напряжения';

                            {Buf[13]..Buf[14]: обороты генератора. }
                            Move(CurrentRecord.Data[13], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + #09+ 'обороты генератора';

                            {Buf[15]..Buf[16]: сопротивление нагрузки. }
                            Move(CurrentRecord.Data[15], w, 2);
                            s:= #09#09#09#09 + Format('%.3f', [w / 1000]) + #09+ 'сопротивление нагрузки';

                            {Buf[17]..Buf[20]: время длит. сред. удара превышающего 50G (*25мкс) по оси Х. }
                            Move(CurrentRecord.Data[17], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 50G (*25мкс)';

                            {Buf[21]: максимум по оси Х. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[21]) + #09+ 'максимум по оси Х';


                            {Buf[22]: максимум по оси Y. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[22]) + #09+ 'максимум по оси Y';

                            {Buf[23]: максимум по оси Z. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[23]) + #09+ 'максимум по оси Z';

                            {Buf[24]: температура силового модуля. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[24]) + '°'#09+ 'температура силового модуля';

                            {Buf[25]..Buf[28]: время длит. сред. удара превышающего 100G (*25мкс)  }
                            Move(CurrentRecord.Data[25], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 100G (*25мкс)';

                            {Buf[29]..Buf[32]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[29], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 150G (*25мкс)';

                             {Buf[33]..Buf[34]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[33], i, 2);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'максимальное количество ударов в секунду в течении минуты';

                          end;
                          {$ENDREGION}

                      06: {$REGION ' версия №06 '}                 {Konovalov 11/01/2018}
                           begin

                            {Buf[07]: min_Uc_min. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[07]) + #09+ 'минимальное напряжение на конденсаторе в минимуме (В)';

                            {Buf[08]: max_Uc_min. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[08]) + #09+ 'максимальное напряжение на конденсаторе в минимуме (В)';

                            {Buf[09]: min_Uc_max. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[09]) + #09+ 'минимальное напряжение на конденсаторе в максимуме (В)';

                            {Buf[10]: max_Uc_max. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[10]) + #09+ 'максимальное напряжение на конденсаторе в максимуме (В)';

                            {Buf[11]: min_Uvh_min . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[11]) + #09+ 'минимальное входное напряжение в минимуме (В)';

                            {Buf[12]: max_Uvh_min . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[12]) + #09+ 'максимальное входное напряжение в минимуме (В)';

                            {Buf[13]: min_Uvh_max . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[13]) + #09+ 'минимальное входное напряжение в максимуме (В)';

                            {Buf[14]: max_Uvh_max . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[14]) + #09+ 'максимальное входное напряжение в максимуме (В)';

                            {Buf[15]: min_vremy_1_go_pika  . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[15]) + #09+ 'минимальное время 1-го пика (мс)';

                            {Buf[16]: max_vremy_1_go_pika  . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[16]) + #09+ 'максимальное время 1-го пика (мс)';

                            {Buf[17]: min_ampl_1_go_pika   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[17]*5) + #09+ 'минимальная амплитуда 1-го пика (мА)';

                            {Buf[18]: max_ampl_1_go_pika   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[18]*5) + #09+ 'максимальная амплитуда 1-го пика (мА)';

                            {Buf[19]: min_vremy_nach_2_go_pika   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[19]) + #09+ 'минимальное время начала 2-го пика (мс)';

                            {Buf[20]: max_vremy_nach_2_go_pika   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[20]) + #09+ 'максимальное время начала 2-го пика (мс)';

                            {Buf[21]: min_ampl_nach_2_go_pika   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[21]*5) + #09+ 'минимальная амплитуда начала 2-го пика (мА)';

                            {Buf[22]: max_ampl_nach_2_go_pika   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[22]*5) + #09+ 'максимальная амплитуда начала 2-го пика (мА)';

                            {Buf[23]: min_vremy_2_go_pika   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[23]) + #09+ 'минимальное время 2-го пика (мс)';

                            {Buf[24]: max_vremy_2_go_pika   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[24]) + #09+ 'максимальное время 2-го пика (мс)';

                            {Buf[25]: min_ampl_2_go_pika   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[25]*5) + #09+ 'минимальная амплитуда 2-го пика (мА)';

                            {Buf[26]: max_ampl_2_go_pika   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[26]*5) + #09+ 'максимальная амплитуда 2-го пика (мА)';

                            {Buf[27]: min_vremy_nach_uderj   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[27]) + #09+ 'минимальное время начала удержания (мс)';

                            {Buf[28]: max_vremy_nach_uderj   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[28]) + #09+ 'максимальное время начала удержания (мс)';

                            {Buf[29]: ct_flip_flop   . }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[29]) + #09+ 'количество ошибок пульсатора';

                            {Buf[30]..Buf[31]: обороты генератора. }
                            Move(CurrentRecord.Data[30], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + #09+ 'обороты генератора (об/мин.)';

                            {Buf[32]..Buf[35]: время длит.  сред. удара превышающего  50G (мкс). }
                            Move(CurrentRecord.Data[32], i, 4);
                            s:= #09#09#09#09 + IntToStr(i*25) + #09+ 'время длит.  сред. удара превышающего 50G (мкс)';

                            {Buf[36]: максимум по оси Х. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[37]) + #09+ 'максимум по оси Х (g)';         //поменяли местами ХУ т.к. перепутали на плате 14.04.20 Коновалов

                            {Buf[37]: максимум по оси Y. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[36]) + #09+ 'максимум по оси Y (g)';

                            {Buf[38]: максимум по оси Z. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[38]) + #09+ 'максимум по оси Z (g)';

                            {Buf[39]..Buf[40]: код телеметрии. }
                            Move(CurrentRecord.Data[39], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + #09+ 'код телеметрии';

                            {Buf[41]..Buf[44]: время длит. сред. удара превышающего 100G (*25мкс)  }
                            Move(CurrentRecord.Data[41], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 100G (*25мкс)';

                            {Buf[45]..Buf[48]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[45], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 150G (*25мкс)';

                             {Buf[45]..Buf[48]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[49], i, 2);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'максимальное количество ударов в секунду в течении минуты';

                      end;
                         {$ENDREGION}

                      07: {$REGION ' версия №07 МС '}
                          begin
                            {Buf[07]..Buf[08]: ток инвертора. }
                            Move(CurrentRecord.Data[07], w, 2);
                            s:= #09#09#09#09 + Format('%.1f', [w / 10]) + #09+ 'ток инвертора';

                            {Buf[09]..Buf[10]: максимум входного напряжения. }
                            Move(CurrentRecord.Data[09], w, 2);
                            s:= #09#09#09#09 + Format('%.1f', [w / 10]) + #09+ 'максимум входного напряжения';

                            {Buf[11]..Buf[12]: минимум входного напряжения. }
                            Move(CurrentRecord.Data[11], w, 2);
                            s:= #09#09#09#09 + Format('%.1f', [w / 10]) + #09+ 'минимум входного напряжения';

                            {Buf[13]..Buf[14]: обороты генератора. }
                            Move(CurrentRecord.Data[13], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + #09+ 'обороты генератора';

                            {Buf[15]..Buf[16]: сопротивление нагрузки. }
                            Move(CurrentRecord.Data[15], w, 2);
                            s:= #09#09#09#09 + Format('%.3f', [w / 100]) + #09+ 'сопротивление нагрузки';

                            {Buf[17]..Buf[20]: время длит. сред. удара превышающего 50G (*25мкс) по оси Х. }
                            Move(CurrentRecord.Data[17], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 50G (*25мкс)';


                            {Buf[21]: максимум по оси Х. }
                            U1:= CurrentRecord.Data[21];         //поменяли местами ХУ т.к. перепутали на плате 14.04.20 Коновалов
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_X'), U1);

                            {Buf[22]: максимум по оси Y. }
                            U1:= CurrentRecord.Data[22];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Y'), U1);

                            {Buf[23]: максимум по оси Z. }
                            U1:= CurrentRecord.Data[23];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Z'), U1);


                            {Buf[24]: температура силового модуля. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[24]) + '°'#09+ 'температура силового модуля';

                            {Buf[25]..Buf[28]: время длит. сред. удара превышающего 100G (*25мкс)  }
                            Move(CurrentRecord.Data[25], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 100G (*25мкс)';

                            {Buf[29]..Buf[32]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[29], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 150G (*25мкс)';

                             {Buf[33]..Buf[34]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[33], i, 2);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'максимальное количество ударов в секунду в течении минуты';

                             {Buf[35]..Buf[36]: осевая вибрация по оси Х  }
                            Move(CurrentRecord.Data[35], U2, 2);
                            F4:= U2 * 0.25;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Vib_X'), F4);

                             {Buf[37]..Buf[38]: осевая вибрация по осям Y,Z  }
                            Move(CurrentRecord.Data[37], U2, 2);
                            F4:= U2 * 0.25;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Vib_X'), F4);
                          end;
                           {$ENDREGION}

                      08: {$REGION ' версия №08 MUP '}                 {Konovalov 11/01/2018}
                           begin
                            {Buf[07]: min_Uc_min. }
                            U1:= CurrentRecord.Data[7];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UCmiVmi'), U1);

                            {Buf[08]: max_Uc_min. }
                            U1:= CurrentRecord.Data[8];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UCmaVmi'), U1);

                            {Buf[09]: min_Uc_max. }
                            U1:= CurrentRecord.Data[9];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UCmiVma'), U1);

                            {Buf[10]: max_Uc_max. }
                            U1:= CurrentRecord.Data[10];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UCmaVma'), U1);

                            {Buf[11]: min_Uvh_min . }
                            U1:= CurrentRecord.Data[11];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UImiVmi'), U1);

                            {Buf[12]: max_Uvh_min . }
                            U1:= CurrentRecord.Data[12];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UImaVmi'), U1);

                            {Buf[13]: min_Uvh_max . }
                            U1:= CurrentRecord.Data[13];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UImiVma'), U1);

                            {Buf[14]: max_Uvh_max . }
                            U1:= CurrentRecord.Data[14];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UImaVma'), U1);

                            {Buf[15]: min_vremy_1_go_pika  . }
                            U1:= CurrentRecord.Data[15];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmin1'), U1);

                            {Buf[16]: max_vremy_1_go_pika  . }
                            U1:= CurrentRecord.Data[16];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmax1'), U1);

                            {Buf[17]: min_ampl_1_go_pika   . }
                            U1:= CurrentRecord.Data[17];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin1'), U1);

                            {Buf[18]: max_ampl_1_go_pika   . }
                            U1:= CurrentRecord.Data[18];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax1'), U1);

                            {Buf[19]: min_vremy_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[19];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmin2st'), U1);

                            {Buf[20]: max_vremy_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[20];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmax2st'), U1);

                            {Buf[21]: min_ampl_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[21];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin2st'), U1);

                            {Buf[22]: max_ampl_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[22];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax2st'), U1);

                            {Buf[23]: min_vremy_2_go_pika   . }
                            U1:= CurrentRecord.Data[23];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmin2'), U1);

                            {Buf[24]: max_vremy_2_go_pika   . }
                            U1:= CurrentRecord.Data[24];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmax2'), U1);

                            {Buf[25]: min_ampl_2_go_pika   . }
                            U1:= CurrentRecord.Data[25];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin2'), U1);

                            {Buf[26]: max_ampl_2_go_pika   . }
                            U1:= CurrentRecord.Data[26];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax2'), U1);

                            {Buf[27]: min_vremy_nach_uderj   . }
                            U1:= CurrentRecord.Data[27];
                            TffFrames.AddData(TffStructure.GetOffsetByName('TminRet'), U1);

                            {Buf[28]: max_vremy_nach_uderj   . }
                            U1:= CurrentRecord.Data[28];
                            TffFrames.AddData(TffStructure.GetOffsetByName('TmaxRet'), U1);

                            {Buf[29]: ct_flip_flop   . }
                            U1:= CurrentRecord.Data[29];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Nerror'), U1);

                            {Buf[30]..Buf[31]: обороты генератора. TRPM}
                            Move(CurrentRecord.Data[30], U2, 2);
                            TffFrames.AddData(TffStructure.GetOffsetByName('TRPM'), U2);

                            {Buf[32]..Buf[35]: время длит.  сред. удара превышающего  50G (мкс). }
                            Move(CurrentRecord.Data[32], I4, 4);
                            TffFrames.AddData(TffStructure.GetOffsetByName('Th50G'), I4);

                            {Buf[36]: максимум по оси Х. }
                            U1:= CurrentRecord.Data[37];         //поменяли местами ХУ т.к. перепутали на плате 14.04.20 Коновалов
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_X'), U1);

                            {Buf[37]: максимум по оси Y. }
                            U1:= CurrentRecord.Data[36];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Y'), U1);

                            {Buf[38]: максимум по оси Z. }
                            U1:= CurrentRecord.Data[38];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Z'), U1);

                            {Buf[39]..Buf[40]: код телеметрии. }
                            Move(CurrentRecord.Data[39], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + #09+ 'код телеметрии';

                            {Buf[41]..Buf[44]: время длит. сред. удара превышающего 100G (*25мкс)  }
                            Move(CurrentRecord.Data[41], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 100G (*25мкс)';

                            {Buf[45]..Buf[48]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[45], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 150G (*25мкс)';

                             {Buf[49]..Buf[50]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[49], i, 2);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'максимальное количество ударов в секунду в течении минуты';

                             {Buf[51]..Buf[52]: осевая вибрация по оси Х  }
                            Move(CurrentRecord.Data[51], U2, 2);
                            F4:= U2 * 0.25;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Vib_X'), F4);

                             {Buf[53]..Buf[54]: осевая вибрация по осям Y,Z  }
                            Move(CurrentRecord.Data[53], U2, 2);
                            F4:= U2 * 0.5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Vib_Lat'), F4);

                      end;
                           {$ENDREGION}

                      10: {$REGION ' версия №08 MUP '}                 {Konovalov 06/06/2022}
                           begin
                            {Buf[07]: min_Uc_min. }
                            U1:= CurrentRecord.Data[7];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UCmiVmi'), U1);

                            {Buf[08]: max_Uc_min. }
                            U1:= CurrentRecord.Data[8];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UCmaVmi'), U1);

                            {Buf[09]: min_Uc_max. }
                            U1:= CurrentRecord.Data[9];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UCmiVma'), U1);

                            {Buf[10]: max_Uc_max. }
                            U1:= CurrentRecord.Data[10];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UCmaVma'), U1);

                            {Buf[11]: min_Uvh_min . }
                            U1:= CurrentRecord.Data[11];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UImiVmi'), U1);

                            {Buf[12]: max_Uvh_min . }
                            U1:= CurrentRecord.Data[12];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UImaVmi'), U1);

                            {Buf[13]: min_Uvh_max . }
                            U1:= CurrentRecord.Data[13];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UImiVma'), U1);

                            {Buf[14]: max_Uvh_max . }
                            U1:= CurrentRecord.Data[14];
                            TffFrames.AddData(TffStructure.GetOffsetByName('UImaVma'), U1);

                            {Buf[15]: min_vremy_1_go_pika  . }
                            U1:= CurrentRecord.Data[15];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmin1'), U1);

                            {Buf[16]: max_vremy_1_go_pika  . }
                            U1:= CurrentRecord.Data[16];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmax1'), U1);

                            {Buf[17]: min_ampl_1_go_pika   . }
                            U1:= CurrentRecord.Data[17];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin1'), U1);

                            {Buf[18]: max_ampl_1_go_pika   . }
                            U1:= CurrentRecord.Data[18];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax1'), U1);

                            {Buf[19]: min_vremy_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[19];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmin2st'), U1);

                            {Buf[20]: max_vremy_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[20];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmax2st'), U1);

                            {Buf[21]: min_ampl_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[21];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin2st'), U1);

                            {Buf[22]: max_ampl_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[22];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax2st'), U1);

                            {Buf[23]: min_vremy_2_go_pika   . }
                            U1:= CurrentRecord.Data[23];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmin2'), U1);

                            {Buf[24]: max_vremy_2_go_pika   . }
                            U1:= CurrentRecord.Data[24];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmax2'), U1);

                            {Buf[25]: min_ampl_2_go_pika   . }
                            U1:= CurrentRecord.Data[25];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin2'), U1);

                            {Buf[26]: max_ampl_2_go_pika   . }
                            U1:= CurrentRecord.Data[26];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax2'), U1);

                            {Buf[27]: min_vremy_nach_uderj   . }
                            U1:= CurrentRecord.Data[27];
                            TffFrames.AddData(TffStructure.GetOffsetByName('TminRet'), U1);

                            {Buf[28]: max_vremy_nach_uderj   . }
                            U1:= CurrentRecord.Data[28];
                            TffFrames.AddData(TffStructure.GetOffsetByName('TmaxRet'), U1);

                            {Buf[29]: ct_flip_flop   . }
                            U1:= CurrentRecord.Data[29];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Nerror'), U1);

                            {Buf[30]..Buf[31]: обороты генератора. }
                            Move(CurrentRecord.Data[30], U2, 2);
                            TffFrames.AddData(TffStructure.GetOffsetByName('TRPM'), U2);

                            {Buf[32]..Buf[35]: время длит.  сред. удара превышающего  50G (мкс). }
                            Move(CurrentRecord.Data[32], I4, 4);
                            TffFrames.AddData(TffStructure.GetOffsetByName('Th50G'), I4);

                            {Buf[36]: максимум по оси Х. }
                            U1:= CurrentRecord.Data[37];         //поменяли местами ХУ т.к. перепутали на плате 14.04.20 Коновалов
                            TffFrames.AddData(TffStructure.GetTFFDataChannels[10].Offset, U1);

                            {Buf[37]: максимум по оси Y. }
                            U1:= CurrentRecord.Data[36];
                            TffFrames.AddData(TffStructure.GetTFFDataChannels[11].Offset, U1);

                            {Buf[38]: максимум по оси Z. }
                            U1:= CurrentRecord.Data[38];
                            TffFrames.AddData(TffStructure.GetTFFDataChannels[12].Offset, U1);

                            {Buf[39]..Buf[40]: код телеметрии. }
                            Move(CurrentRecord.Data[39], w, 2);
                            s:= #09#09#09#09 + IntToStr(w) + #09+ 'код телеметрии';

                            {Buf[41]..Buf[44]: время длит. сред. удара превышающего 100G (*25мкс)  }
                            Move(CurrentRecord.Data[41], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 100G (*25мкс)';

                            {Buf[45]..Buf[48]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[45], i, 4);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'время длит. сред. удара превышающего 150G (*25мкс)';

                             {Buf[49]..Buf[50]: время длит. сред. удара превышающего 150G (*25мкс)  }
                            Move(CurrentRecord.Data[49], i, 2);
                            s:= #09#09#09#09 + IntToStr(i) + #09+ 'максимальное количество ударов в секунду в течении минуты';

                              {Buf[51]..Buf[52]: осевая вибрация по оси Х  }
                            Move(CurrentRecord.Data[51], U2, 2);
                            F4:= U2 * 0.25;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Vib_X'), F4);

                             {Buf[53]..Buf[54]: осевая вибрация по осям Y,Z  }
                            Move(CurrentRecord.Data[53], U2, 2);
                            F4:= U2 * 0.5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Vib_Lat'), F4);

                             {Buf[55]: температура МУП. }
                            I1:= CurrentRecord.Data[55];
                            TffFrames.AddData(TffStructure.GetOffsetByName('MUP_Temp'), I1);

                      end;
                           {$ENDREGION}

                    end;
                  end;
                  {$ENDREGION}

                    {$ENDREGION}

              42: {$REGION ' запись динамического замера '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]         : № версии для записи 42.
                     Buf[07]..Buf[21]: данные.                 }

                   {далее интерпретируем содержимое записи в зав. от номера её версии. }
                    case CurrentRecord.Data[6] of                                   {в новом формате записей в байте [6] всегда лежит № версии                      }
                      00, 02: {$REGION ' версия №00 '}
                          begin
                            {Buf[07]..Buf[08]: показания акселерометра AX. }
                            Move(CurrentRecord.Data[07], I2, 2);
                            F4:= I2 * 1.2 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_GX'), F4);

                            {Buf[09]..Buf[10]: показания акселерометра AY. }
                            Move(CurrentRecord.Data[09], I2, 2);
                            F4:= I2 * 1.2 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_GY'), F4);

                            {Buf[11]..Buf[12]: показания акселерометра AZ. }
                            Move(CurrentRecord.Data[11], I2, 2);
                            F4:= I2 * 1.2 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_GZ'), F4);

                            {Buf[13]..Buf[14]: показания магнитометра BX. }
                            Move(CurrentRecord.Data[13], I2, 2);
                            F4:= I2 * 1200 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_HX'), F4);

                            {Buf[15]..Buf[16]: показания магнитометра BY. }
                            Move(CurrentRecord.Data[15], I2, 2);
                            F4:= I2 * 1200 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_HY'), F4);

                            {Buf[17]..Buf[18]: показания магнитометра BZ. }
                            Move(CurrentRecord.Data[17], I2, 2);
                            F4:= I2 * 1200 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_HZ'), F4);

                            {Buf[19]: скорость буровой колонны об/мин. }
                            U1:= CurrentRecord.Data[19];
                            TffFrames.AddData(TffStructure.GetOffsetByName('CRPM'), U1);

                            {Buf[20]: температура модуля инклинометра. }
                            I1:= CurrentRecord.Data[20];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Incl_Temp'), I1);

                            {Buf[21]: неравномерность вращения. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[21]) + #09+ 'неравномерность вращения';
                          end;
                      01: {$REGION ' версия №01 '}
                          begin
                            {Buf[07]..Buf[08]: показания акселерометра AX. }
                            Move(CurrentRecord.Data[07], I2, 2);
                            F4:= I2 * 1.2 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_GX'), F4);

                            {Buf[09]..Buf[10]: показания акселерометра AY. }
                            Move(CurrentRecord.Data[09], I2, 2);
                            F4:= I2 * 1.2 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_GY'), F4);

                            {Buf[11]..Buf[12]: показания акселерометра AZ. }
                            Move(CurrentRecord.Data[11], I2, 2);
                            F4:= I2 * 1.2 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_GZ'), F4);

                            {Buf[13]..Buf[14]: показания магнитометра BX. }
                            Move(CurrentRecord.Data[13], I2, 2);
                            F4:= I2 * 1200 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_HX'), F4);

                            {Buf[15]..Buf[16]: показания магнитометра BY. }
                            Move(CurrentRecord.Data[15], I2, 2);
                            F4:= I2 * 1200 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_HY'), F4);

                            {Buf[17]..Buf[18]: показания магнитометра BZ. }
                            Move(CurrentRecord.Data[17], I2, 2);
                            F4:= I2 * 1200 / $7FFF;
                            TffFrames.AddData(TffStructure.GetOffsetByName('d_HZ'), F4);

                            {Buf[19]: скорость буровой колонны об/мин. }
                            U1:= CurrentRecord.Data[19];
                            TffFrames.AddData(TffStructure.GetOffsetByName('CRPM'), U1);

                            {Buf[20]: температура модуля инклинометра. }
                            I1:= CurrentRecord.Data[20];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Incl_Temp'), I1);

                            {Buf[21]: неравномерность вращения. }
                            s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[21]) + #09+ 'неравномерность вращения';

                             {Buf[22]: время удара >50G }
                            Move(CurrentRecord.Data[22], i, 4);
                            s:= #09#09#09#09 + IntToStr(i*100) + ' мкс'#09+ 'время удара >50G';

                            {Buf[26]: максимум по оси Х. }
                            U1:= CurrentRecord.Data[27];         //поменяли местами ХУ т.к. перепутали на плате 14.04.20 Коновалов
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_X'), U1);

                            {Buf[27]: максимум по оси Y. }
                            U1:= CurrentRecord.Data[26];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Y'), U1);

                            {Buf[28]: максимум по оси Z. }
                            U1:= CurrentRecord.Data[28];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Z'), U1);

                          end;
                          {$ENDREGION}
                    end;
                  end;
                  {$ENDREGION}
            end;  // case

        end;  { IsValidDateTime }
      end;  { CurrentRecord.N }
    end; { b = $C0 }
  until EndOfFile Or (ErrorCode > 0);
  Result:= TffFrames.GetFrameRecords;
  TffFrames.Done;
end;

end.


