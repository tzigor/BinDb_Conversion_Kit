unit ParseBin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DateUtils,
  Utils, UserTypes, TffObjects;

  procedure BinDataChannelsSet(TFF_Ver: Byte);
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


procedure BinDataChannelsSet(Tff_Ver: Byte);
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

  TffStructure.AddChannel('Amin1', 'mA', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('Amax1', 'mA', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('Amin2st', 'mA', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('Amax2st', 'mA', 'U2', '1', Tff_Ver);

  TffStructure.AddChannel('Tmin2', 'ms', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('Tmax2', 'ms', 'U1', '1', Tff_Ver);

  TffStructure.AddChannel('TminRet', 'ms', 'U1', '1', Tff_Ver);
  TffStructure.AddChannel('TmaxRet', 'ms', 'U1', '1', Tff_Ver);

  TffStructure.AddChannel('Amin2', 'mA', 'U2', '1', Tff_Ver);
  TffStructure.AddChannel('Amax2', 'mA', 'U2', '1', Tff_Ver);

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
  i             : Integer;
  w             : Word;

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

  ValidDateTime : Boolean;
  y, m, d, h, n, s : longInt;
  Flag          : Boolean;
  FirstRecord   : Boolean;

begin
  ErrorCode:= WRONG_FILE_FORMAT;

  //AssignFile(FS, 'DebugFile.txt');
  //Rewrite(FS);

  PrevDateTime:= 0;
  TffFrames.Init;
  DataOffset:= 0;
  EndOfFile:= False;
  FirstRecord:= True;

  repeat
    b:= 0;
    while (b <> $C0) And (Not EndOfFile) do b:= GetCurrentByte;

    if b = $C0 then begin
      CurrentRecord:= GetCurrentRecord;
      if  CurrentRecord.N > 0 then begin
        try
           Flag:= TryStrToInt(IntToHex(CurrentRecord.Data[0]), y);
           Flag:= TryStrToInt(IntToHex(CurrentRecord.Data[1]), m);
           Flag:= TryStrToInt(IntToHex(CurrentRecord.Data[2]), d);
           Flag:= TryStrToInt(IntToHex(CurrentRecord.Data[3]), h);
           Flag:= TryStrToInt(IntToHex(CurrentRecord.Data[4]), n);
           Flag:= TryStrToInt(IntToHex(CurrentRecord.Data[5]), s);
           if flag  And (y >= 20) And (y <= 30) then ValidDateTime:= IsValidDateTime(2000 + y, m, d, h, n, s, 0)
           else ValidDateTime:= False;
        except
           ValidDateTime:= False;
        end;
        if ValidDateTime then
        begin
            DateTime:= EncodeDateTime(2000 + y, m, d, h, n, s, 0);
            if FirstRecord then begin
               TffFrames.AddRecord(DateTime, TffStructure.GetDataChannelSize, TffStructure.GetTFFDataChannels);
               PrevDateTime:= DateTime;
               FirstRecord:= False;
            end;
            if (PrevDateTime <> DateTime) And (DaysBetween(PrevDateTime, DateTime) < 50) then TffFrames.AddRecord(DateTime, TffStructure.GetDataChannelSize, TffStructure.GetTFFDataChannels);
            PrevDateTime:= DateTime;
            ErrorCode:= NO_ERROR;

            case CurrentRecord.Cmd of
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
                    //s:= #09#09#09#09 + IntToStr(I2) + #09#09 + 'приращение отклонителя';

                    {Buf[20]..Buf[21]: максимум пульсаций акселерометров. }
                    Move(CurrentRecord.Data[20], w, 2);
                    //s:= #09#09#09#09 + IntToStr(w) + #09#09 + 'максимум пульсаций акселерометров';
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

              41: {$REGION ' запись параметров передачи силового модуля - новый формат ударов (бывш. 06) '}
                  begin
                    {Buf[00]..Buf[05]: дата и время в формате BCD.
                     Buf[06]         : № версии для записи 41.
                     Buf[07]..Buf[35]: данные.                 }

                    case CurrentRecord.Data[6] of
                      05: {$REGION ' версия №05 '}
                        begin
                            {Buf[13]..Buf[14]: обороты генератора. TRPM}
                            Move(CurrentRecord.Data[13], U2, 2);
                            TffFrames.AddData(TffStructure.GetOffsetByName('TRPM'), U2);

                            {Buf[17]..Buf[20]: время длит.  сред. удара превышающего  50G (мкс). }
                            Move(CurrentRecord.Data[17], I4, 4);
                            TffFrames.AddData(TffStructure.GetOffsetByName('Th50G'), I4);

                            {Buf[21]: максимум по оси Х. }
                            U1:= CurrentRecord.Data[21];         //поменяли местами ХУ т.к. перепутали на плате 14.04.20 Коновалов
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_X'), U1);

                            {Buf[22]: максимум по оси Y. }
                            U1:= CurrentRecord.Data[22];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Y'), U1);

                            {Buf[23]: максимум по оси Z. }
                            U1:= CurrentRecord.Data[23];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Z'), U1);
;
                            {Buf[24]: температура МУП. }
                            I1:= CurrentRecord.Data[24];
                            TffFrames.AddData(TffStructure.GetOffsetByName('MUP_Temp'), I1);

                        end;
                        {$ENDREGION}

                    06: {$REGION ' версия №06 '}                 {Konovalov 11/01/2018}
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
                          U2:= CurrentRecord.Data[17] * 5;
                          TffFrames.AddData(TffStructure.GetOffsetByName('Amin1'), U2);

                          {Buf[18]: max_ampl_1_go_pika   . }
                          U2:= CurrentRecord.Data[18] * 5;
                          TffFrames.AddData(TffStructure.GetOffsetByName('Amax1'), U2);

                          {Buf[19]: min_vremy_nach_2_go_pika   . }
                          U1:= CurrentRecord.Data[19];
                          TffFrames.AddData(TffStructure.GetOffsetByName('Tmin2st'), U1);

                          {Buf[20]: max_vremy_nach_2_go_pika   . }
                          U1:= CurrentRecord.Data[20];
                          TffFrames.AddData(TffStructure.GetOffsetByName('Tmax2st'), U1);

                          {Buf[21]: min_ampl_nach_2_go_pika   . }
                          U2:= CurrentRecord.Data[21] * 5;
                          TffFrames.AddData(TffStructure.GetOffsetByName('Amin2st'), U2);

                          {Buf[22]: max_ampl_nach_2_go_pika   . }
                          U2:= CurrentRecord.Data[22] * 5;
                          TffFrames.AddData(TffStructure.GetOffsetByName('Amax2st'), U2);

                          {Buf[23]: min_vremy_2_go_pika   . }
                          U1:= CurrentRecord.Data[23];
                          TffFrames.AddData(TffStructure.GetOffsetByName('Tmin2'), U1);

                          {Buf[24]: max_vremy_2_go_pika   . }
                          U1:= CurrentRecord.Data[24];
                          TffFrames.AddData(TffStructure.GetOffsetByName('Tmax2'), U1);

                          {Buf[25]: min_ampl_2_go_pika   . }
                          U2:= CurrentRecord.Data[25] * 5;
                          TffFrames.AddData(TffStructure.GetOffsetByName('Amin2'), U2);

                          {Buf[26]: max_ampl_2_go_pika   . }
                          U2:= CurrentRecord.Data[26] * 5;
                          TffFrames.AddData(TffStructure.GetOffsetByName('Amax2'), U2);

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
                          U1:= CurrentRecord.Data[36];         //поменяли местами ХУ т.к. перепутали на плате 14.04.20 Коновалов
                          TffFrames.AddData(TffStructure.GetOffsetByName('Shock_X'), U1);

                          {Buf[37]: максимум по оси Y. }
                          U1:= CurrentRecord.Data[37];
                          TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Y'), U1);

                          {Buf[38]: максимум по оси Z. }
                          U1:= CurrentRecord.Data[38];
                          TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Z'), U1);

                         end;

                       {$ENDREGION}

                    07: {$REGION ' версия №07 МС '}
                        begin

                          {Buf[13]..Buf[14]: обороты генератора. TRPM}
                          Move(CurrentRecord.Data[13], U2, 2);
                          TffFrames.AddData(TffStructure.GetOffsetByName('TRPM'), U2);

                          {Buf[17]..Buf[20]: время длит.  сред. удара превышающего  50G (мкс). }
                          Move(CurrentRecord.Data[17], I4, 4);
                          TffFrames.AddData(TffStructure.GetOffsetByName('Th50G'), I4);

                          {Buf[21]: максимум по оси Х. }
                          U1:= CurrentRecord.Data[22];         //поменяли местами ХУ т.к. перепутали на плате 14.04.20 Коновалов
                          TffFrames.AddData(TffStructure.GetOffsetByName('Shock_X'), U1);

                          {Buf[22]: максимум по оси Y. }
                          U1:= CurrentRecord.Data[23];
                          TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Y'), U1);

                          {Buf[23]: максимум по оси Z. }
                          U1:= CurrentRecord.Data[23];
                          TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Z'), U1);

                          {Buf[24]: температура МУП. }
                          I1:= CurrentRecord.Data[24];
                          TffFrames.AddData(TffStructure.GetOffsetByName('MUP_Temp'), I1);

                          {Buf[35]..Buf[36]: осевая вибрация по оси Х  }
                          Move(CurrentRecord.Data[35], U2, 2);
                          F4:= U2 * 0.25;
                          TffFrames.AddData(TffStructure.GetOffsetByName('Vib_X'), F4);

                           {Buf[37]..Buf[38]: осевая вибрация по осям Y,Z  }
                          Move(CurrentRecord.Data[38], U2, 2);
                          F4:= U2 * 0.5;
                          TffFrames.AddData(TffStructure.GetOffsetByName('Vib_Lat'), F4);
                         end;

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
                            U2:= CurrentRecord.Data[17] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin1'), U2);

                            {Buf[18]: max_ampl_1_go_pika   . }
                            U2:= CurrentRecord.Data[18] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax1'), U2);

                            {Buf[19]: min_vremy_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[19];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmin2st'), U1);

                            {Buf[20]: max_vremy_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[20];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmax2st'), U1);

                            {Buf[21]: min_ampl_nach_2_go_pika   . }
                            U2:= CurrentRecord.Data[21] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin2st'), U2);

                            {Buf[22]: max_ampl_nach_2_go_pika   . }
                            U2:= CurrentRecord.Data[22] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax2st'), U2);

                            {Buf[23]: min_vremy_2_go_pika   . }
                            U1:= CurrentRecord.Data[23];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmin2'), U1);

                            {Buf[24]: max_vremy_2_go_pika   . }
                            U1:= CurrentRecord.Data[24];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmax2'), U1);

                            {Buf[25]: min_ampl_2_go_pika   . }
                            U2:= CurrentRecord.Data[25] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin2'), U2);

                            {Buf[26]: max_ampl_2_go_pika   . }
                            U2:= CurrentRecord.Data[26] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax2'), U2);

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
                            U2:= CurrentRecord.Data[17] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin1'), U2);

                            {Buf[18]: max_ampl_1_go_pika   . }
                            U2:= CurrentRecord.Data[18] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax1'), U2);

                            {Buf[19]: min_vremy_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[19];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmin2st'), U1);

                            {Buf[20]: max_vremy_nach_2_go_pika   . }
                            U1:= CurrentRecord.Data[20];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmax2st'), U1);

                            {Buf[21]: min_ampl_nach_2_go_pika   . }
                            U2:= CurrentRecord.Data[21] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin2st'), U2);

                            {Buf[22]: max_ampl_nach_2_go_pika   . }
                            U2:= CurrentRecord.Data[22] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax2st'), U2);

                            {Buf[23]: min_vremy_2_go_pika   . }
                            U1:= CurrentRecord.Data[23];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmin2'), U1);

                            {Buf[24]: max_vremy_2_go_pika   . }
                            U1:= CurrentRecord.Data[24];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Tmax2'), U1);

                            {Buf[25]: min_ampl_2_go_pika   . }
                            U2:= CurrentRecord.Data[25] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amin2'), U2);

                            {Buf[26]: max_ampl_2_go_pika   . }
                            U2:= CurrentRecord.Data[26] * 5;
                            TffFrames.AddData(TffStructure.GetOffsetByName('Amax2'), U2);

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
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_X'), U1);

                            {Buf[37]: максимум по оси Y. }
                            U1:= CurrentRecord.Data[36];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Y'), U1);

                            {Buf[38]: максимум по оси Z. }
                            U1:= CurrentRecord.Data[38];
                            TffFrames.AddData(TffStructure.GetOffsetByName('Shock_Z'), U1);

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
                            //s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[21]) + #09+ 'неравномерность вращения';
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
                            //s:= #09#09#09#09 + IntToStr(CurrentRecord.Data[21]) + #09+ 'неравномерность вращения';

                             {Buf[22]: время удара >50G }
                            Move(CurrentRecord.Data[22], i, 4);
                            //s:= #09#09#09#09 + IntToStr(i*100) + ' мкс'#09+ 'время удара >50G';

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
  until EndOfFile Or ((ErrorCode > 0) And ValidDateTime);
  if TffFrames.GetFrameRecords = Nil then ErrorCode:= WRONG_FILE_FORMAT;
  Result:= TffFrames.GetFrameRecords;
  TffFrames.Done;
end;

end.


