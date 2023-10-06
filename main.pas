unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Spin, DateUtils, StrUtils, Utils, UserTypes, ParseBin, TffObjects,
  BIN_DB_Converter, CSV_Converter, Buttons, LCLType, ExtCtrls, ParseGam2,
  ParseGam, ParseLTB, ConvertVersion, ParseBinDb;

type

  { TApp }

  TApp = class(TForm)
    AddParameters: TCheckBox;
    AddMeasure: TCheckBox;
    AddUnitsFlag: TCheckBox;
    AddTypeFlag: TCheckBox;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    RecordRateL: TLabel;
    millisecL: TLabel;
    CloseApp: TBitBtn;
    FileTypeList: TComboBox;
    GroupBox1: TGroupBox;
    RawSource: TRadioButton;
    BinDbSource: TRadioButton;
    RecordRate: TSpinEdit;
    SortTime: TCheckBox;
    ConvertToTxt: TButton;
    ConvertFile: TButton;
    ConvertToCSV: TButton;
    CSVBox: TGroupBox;
    BinDbBox: TGroupBox;
    HumanTime: TRadioButton;
    Label1: TLabel;
    Label5: TLabel;
    ADVSupport: TLabel;
    Indicator: TLabel;
    ProcessLabel: TLabel;
    ProcessProgress: TProgressBar;
    S100: TRadioButton;
    ShiftHr: TSpinEdit;
    ShiftMin: TSpinEdit;
    ShiftSec: TSpinEdit;
    TxtLength: TSpinEdit;
    UnixTime: TRadioButton;
    TimeEx: TLabel;
    TempHead: TLabel;
    TimeHead: TLabel;
    Label4: TLabel;
    NumberEx: TLabel;
    SeparatorChar: TEdit;
    Label2: TLabel;
    FloatDigits: TSpinEdit;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    MainTab: TTabSheet;
    VersionList: TComboBox;
    procedure AddMeasureChange(Sender: TObject);
    procedure AddParametersChange(Sender: TObject);
    procedure AddTypeFlagChange(Sender: TObject);
    procedure AddUnitsFlagChange(Sender: TObject);
    procedure BinDbSourceChange(Sender: TObject);
    procedure CloseAppClick(Sender: TObject);
    procedure ConvertFileClick(Sender: TObject);
    procedure ConvertToCSVClick(Sender: TObject);
    procedure ConvertToTxtClick(Sender: TObject);
    procedure FileTypeListClick(Sender: TObject);
    procedure FloatDigitsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HumanTimeChange(Sender: TObject);
    procedure RawSourceChange(Sender: TObject);
    procedure S100Change(Sender: TObject);
    procedure SeparatorCharChange(Sender: TObject);
    procedure UnixTimeChange(Sender: TObject);
    procedure VersionListChange(Sender: TObject);
  private

  public

  end;

var
  App: TApp;

  ErrorCode          : Byte;

  TFFVersion         : Byte;

  { Data souce current parameters }
  CurrentOpenedFile  : String;
  Bytes              : TBytes;   // Data source
  CurrentFileSize    : LongWord;
  EndOfFile          : Boolean;
  DataOffset         : LongWord;

  TffStructure       : TTffStructure;
  FrameRecords       : TFrameRecords;
  BinDbConverter     : TBinDbConverter;
  CSVConverter       : TCSVConverter;

  ShowIndicator      : Boolean;

implementation

{$R *.lfm}

{ TApp }

procedure TApp.FormCreate(Sender: TObject);
begin
  DecimalSeparator:= '.';
  ADVSupport.Caption:= '';
  TFFVersion:= TFF_V30; // By default
  NumberEx.Caption:= FloatToStrF(ExampleValue, ffFixed, 10, FloatDigits.Value);
  Indicator.Caption:= '';
  ProcessProgress.Visible:= False;
  ProcessLabel.Caption:= '';
  ShowIndicator:= True;
end;

procedure TApp.CloseAppClick(Sender: TObject);
begin
  App.Close;
end;

procedure SortFrameRecordsByTime;
var
  p, q , n: longWord;
  index  : TDateTime;
  vTmp: TFrameRecord;

begin
  n:= Length(FrameRecords);
  ProgressInit(n, 'Sorting');
  for p := 0 to n - 1 do
  begin
    index:= FrameRecords[p].DateTime;
    q:= p;
    while (q > 0) AND (CompareDateTime(FrameRecords[q - 1].DateTime, index) > 0) do
    begin
       vTmp:= FrameRecords[q];
       FrameRecords[q]:= FrameRecords[q - 1];
       FrameRecords[q - 1]:= vTmp;
       Dec(q);
    end;
    FrameRecords[q].DateTime:= index;
    App.ProcessProgress.Position:= p;
  end;
  ProgressDone;
end;

procedure BinToBin_Db();
begin
  if LoadSourceFile('bin', 100) then begin
     BinDataChannelsSet(TFFVersion);
     FrameRecords:= BinDbParser(TFFVersion);


     //FrameRecords:= BinParser;
     //if App.SortTime.Checked then SortFrameRecordsByTime;
     //if ErrorCode = NO_ERROR then begin
     //
     //    BinDbConverter.Init(TFFVersion, FrameRecords);
     //    BinDbConverter.SetTimeShiftByUser(App.ShiftHr.Value, App.ShiftMin.Value, App.ShiftSec.Value);
     //    BinDbConverter.CreateParameters();
     //    BinDbConverter.AddParameter('FORMAT=PC');
     //    BinDbConverter.AddParameter('TYPE=TFF');
     //    BinDbConverter.AddParameter('ToolId type=SIB');
     //    BinDbConverter.AddParameter('ToolId MfgCode=SIB');
     //    BinDbConverter.Composer(TffStructure.GetTFFDataChannels, FrameRecords);
     //
     //    SaveByteArray(BinDbConverter.GetBinDbData, ReplaceText(CurrentOpenedFile,ExtractFileExt(CurrentOpenedFile),'') + '.bin_db');
     //
     //    BinDbConverter.Done;
     //end
     //else Application.MessageBox(GetErrorMessage(ErrorCode),'Error', MB_ICONERROR + MB_OK);
  end;
end;

procedure Gam2ToBin_Db();
begin
  if LoadSourceFile('gam2', 100) then begin
     Gam2DataChannelsSet(TFFVersion);
     FrameRecords:= Gam2Parser;
     if App.SortTime.Checked then SortFrameRecordsByTime;
     if ErrorCode = NO_ERROR then begin

         BinDbConverter.Init(TFFVersion, FrameRecords);
         BinDbConverter.SetTimeShiftByUser(App.ShiftHr.Value, App.ShiftMin.Value, App.ShiftSec.Value);
         BinDbConverter.CreateParameters();
         BinDbConverter.Composer(TffStructure.GetTFFDataChannels, FrameRecords);

         SaveByteArray(BinDbConverter.GetBinDbData, ReplaceText(CurrentOpenedFile,ExtractFileExt(CurrentOpenedFile),'') + '.bin_db');

         BinDbConverter.Done;
     end
     else Application.MessageBox(GetErrorMessage(ErrorCode),'Error', MB_ICONERROR + MB_OK);
  end;
end;

procedure GamToBin_Db();
begin
  if LoadSourceFile('gam', 100) then begin
     GamDataChannelsSet(TFFVersion);
     FrameRecords:= GamParser;
     if App.SortTime.Checked then SortFrameRecordsByTime;
     if ErrorCode = NO_ERROR then begin

         BinDbConverter.Init(TFFVersion, FrameRecords);
         BinDbConverter.SetTimeShiftByUser(App.ShiftHr.Value, App.ShiftMin.Value, App.ShiftSec.Value);
         BinDbConverter.CreateParameters();
         BinDbConverter.Composer(TffStructure.GetTFFDataChannels, FrameRecords);

         SaveByteArray(BinDbConverter.GetBinDbData, ReplaceText(CurrentOpenedFile,ExtractFileExt(CurrentOpenedFile),'') + '.bin_db');

         BinDbConverter.Done;
     end
     else Application.MessageBox(GetErrorMessage(ErrorCode),'Error', MB_ICONERROR + MB_OK);
  end;
end;

procedure LTBToBin_Db();
begin
  if LoadSourceFile('ltb', 100) then begin
     LTBDataChannelsSet(TFFVersion);
     FrameRecords:= LTBParser;
     SortFrameRecordsByTime;
     if ErrorCode = NO_ERROR then begin

         BinDbConverter.Init(TFFVersion, FrameRecords);
         BinDbConverter.SetTimeShiftByUser(App.ShiftHr.Value, App.ShiftMin.Value, App.ShiftSec.Value);
         BinDbConverter.CreateParameters();
         BinDbConverter.Composer(TffStructure.GetTFFDataChannels, FrameRecords);

         SaveByteArray(BinDbConverter.GetBinDbData, ReplaceText(CurrentOpenedFile,ExtractFileExt(CurrentOpenedFile),'') + '.bin_db');

         BinDbConverter.Done;
     end
     else Application.MessageBox(GetErrorMessage(ErrorCode),'Error', MB_ICONERROR + MB_OK);
  end;
end;

procedure TApp.ConvertFileClick(Sender: TObject);
begin
  Indicator.Caption:= '';
  Indicator.Refresh;
  if RawSource.Checked then begin
     if FileTypeList.ItemIndex = 0 then BinToBin_Db;
     if FileTypeList.ItemIndex = 1 then Gam2ToBin_Db;
     if FileTypeList.ItemIndex = 2 then GamToBin_Db;
     if FileTypeList.ItemIndex = 3 then LTBToBin_Db;
  end
  else ConvertBinDbVersion;
end;

procedure TApp.ConvertToCSVClick(Sender: TObject);
var Date_Time_Type: Byte;
begin
  if LoadSourceFile('bin_db', 100) then begin
     if HumanTime.Checked then Date_Time_Type:= 1;
     if UnixTime.Checked then Date_Time_Type:= 2;
     if S100.Checked then Date_Time_Type:= 3;

     CSVConverter.Init(SeparatorChar.Text[1],
                       AddParameters.Checked,
                       AddMeasure.Checked,
                       AddUnitsFlag.Checked,
                       AddTypeFlag.Checked,
                       Date_Time_Type);

     CSVConverter.CSVComposer;
     try
       CSVConverter.GetCSVData.SaveToFile(ReplaceText(CurrentOpenedFile,ExtractFileExt(CurrentOpenedFile),'') + '.csv');
       App.Indicator.Caption:= 'File converted';
     except
       Application.MessageBox('Unable to save CSV file. Probably it is being used by another process','Error', MB_ICONERROR + MB_OK);
     end;
     CSVConverter.Done;
     ProgressDone;
  end;
end;

procedure TApp.ConvertToTxtClick(Sender: TObject);
var Date_Time_Type: Byte;
begin
  if LoadSourceFile('bin_db', 100) then begin
     if HumanTime.Checked then Date_Time_Type:= 1;
     if UnixTime.Checked then Date_Time_Type:= 2;
     if S100.Checked then Date_Time_Type:= 3;

     CSVConverter.Init('',
                       AddParameters.Checked,
                       AddMeasure.Checked,
                       AddUnitsFlag.Checked,
                       AddTypeFlag.Checked,
                       Date_Time_Type);

     CSVConverter.SetItemLength(TxtLength.Value);
     CSVConverter.CSVComposer;
     try
       CSVConverter.GetCSVData.SaveToFile(ReplaceText(CurrentOpenedFile, ExtractFileExt(CurrentOpenedFile), '') + '.txt');
       App.Indicator.Caption:= 'File converted';
     except
       Application.MessageBox('Unable to save TXT file. Probably it is being used by another process','Error', MB_ICONERROR + MB_OK);
     end;
     CSVConverter.Done;
     ProgressDone;
  end;
end;

procedure TApp.FileTypeListClick(Sender: TObject);
begin
  RawSource.Checked:= True;
end;

procedure TApp.FloatDigitsChange(Sender: TObject);
begin
  NumberEx.Caption:= FloatToStrF(ExampleValue, ffFixed, 10, FloatDigits.Value);
end;


procedure TApp.SeparatorCharChange(Sender: TObject);
begin
  if SeparatorChar.Text = '' then SeparatorChar.Text:= ';';
end;

procedure TApp.AddUnitsFlagChange(Sender: TObject);
begin
  CSVConverter.SetAddUnits(AddUnitsFlag.Checked);
  TimeHead.Caption:= 'TIME';
  TempHead.Caption:= 'CTRL_Temp';
  if AddUnitsFlag.Checked then begin
     TimeHead.Caption:= TimeHead.Caption + '(100S)';
     TempHead.Caption:= TempHead.Caption + '(C)';
  end;
  if AddTypeFlag.Checked then begin
     TimeHead.Caption:= TimeHead.Caption + '(F4)';
     TempHead.Caption:= TempHead.Caption + '(I1)';
  end;
end;

procedure TApp.AddTypeFlagChange(Sender: TObject);
begin
  CSVConverter.SetAddType(AddTypeFlag.Checked);
  TimeHead.Caption:= 'TIME';
  TempHead.Caption:= 'CTRL_Temp';
  if AddUnitsFlag.Checked then begin
     TimeHead.Caption:= TimeHead.Caption + '(100S)';
     TempHead.Caption:= TempHead.Caption + '(C)';
  end;
  if AddTypeFlag.Checked then begin
     TimeHead.Caption:= TimeHead.Caption + '(F4)';
     TempHead.Caption:= TempHead.Caption + '(I1)';
  end;
end;

procedure TApp.AddParametersChange(Sender: TObject);
begin
   CSVConverter.SetIncludeParams(AddParameters.Checked);
end;

procedure TApp.AddMeasureChange(Sender: TObject);
begin
  CSVConverter.SetIncludeMeasures(AddMeasure.Checked);
end;

procedure TApp.UnixTimeChange(Sender: TObject);
begin
  if UnixTime.Checked then
    CSVConverter.SetDateTimeType(2);
  TimeEx.Caption:= '1666337309';
end;

procedure TApp.HumanTimeChange(Sender: TObject);
begin
  if HumanTime.Checked then
    CSVConverter.SetDateTimeType(1);
  TimeEx.Caption:= '12/31/2023 23:55:35';
end;

procedure TApp.BinDbSourceChange(Sender: TObject);
begin
  RecordRateL.Visible:= True;
  millisecL.Visible:= True;
  RecordRate.Visible:= True;
end;

procedure TApp.RawSourceChange(Sender: TObject);
begin
  RecordRateL.Visible:= False;
  millisecL.Visible:= False;
  RecordRate.Visible:= False;
end;

procedure TApp.S100Change(Sender: TObject);
begin
  if S100.Checked then CSVConverter.SetDateTimeType(3);
  TimeEx.Caption:= '178.125';
end;

procedure TApp.VersionListChange(Sender: TObject);
begin
   TFFVersion:= VersionList.ItemIndex + 2;
   if TFFVersion = 4 then ADVSupport.Caption:= 'Not supported by ADV'
   else ADVSupport.Caption:= ''
end;

end.

