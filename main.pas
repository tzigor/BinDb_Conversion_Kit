unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Spin, DateUtils, StrUtils, Utils, UserTypes, ParseBin, TffObjects,
  BIN_DB_Converter, CSV_Converter, Buttons, LCLType;

type

  { TApp }

  TApp = class(TForm)
    AddParameters: TCheckBox;
    AddMeasure: TCheckBox;
    AddUnitsFlag: TCheckBox;
    AddTypeFlag: TCheckBox;
    ConvertToCSV: TButton;
    CSVBox: TGroupBox;
    HumanTime: TRadioButton;
    S100: TRadioButton;
    UnixTime: TRadioButton;
    TimeEx: TLabel;
    TempHead: TLabel;
    TimeHead: TLabel;
    Label4: TLabel;
    NumberEx: TLabel;
    SeparatorChar: TEdit;
    Label2: TLabel;
    FloatDigits: TSpinEdit;
    Test: TButton;
    Label1: TLabel;
    VersionList: TComboBox;
    ConvertFile: TButton;
    CloseApp: TButton;
    FileName: TEdit;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    MainTab: TTabSheet;
    procedure AddMeasureChange(Sender: TObject);
    procedure AddParametersChange(Sender: TObject);
    procedure AddTypeFlagChange(Sender: TObject);
    procedure AddUnitsFlagChange(Sender: TObject);
    procedure CloseAppClick(Sender: TObject);
    procedure ConvertFileClick(Sender: TObject);
    procedure ConvertToCSVClick(Sender: TObject);
    procedure FloatDigitsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HumanTimeChange(Sender: TObject);
    procedure S100Change(Sender: TObject);
    procedure SeparatorCharChange(Sender: TObject);
    procedure TestClick(Sender: TObject);
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

implementation

{$R *.lfm}

{ TApp }

procedure TApp.CloseAppClick(Sender: TObject);
begin
  App.Close;
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

procedure TApp.ConvertFileClick(Sender: TObject);
begin
  if LoadSourceFile('bin', 100) then begin
     FileName.Text:= CurrentOpenedFile;

     TFFDataChannelsSet(TFFVersion);
     FrameRecords:= BinParser;
     BinDbConverter.Init(TFFVersion, FrameRecords);

     BinDbConverter.CreateParameters();
     BinDbConverter.AddParameter('FORMAT=PC');
     BinDbConverter.AddParameter('TYPE=TFF');
     BinDbConverter.AddParameter('ToolId type=SIB');
     BinDbConverter.AddParameter('ToolId MfgCode=SIB');

     BinDbConverter.ChannelsComposer(TffStructure.GetTFFDataChannels);

     BinDbConverter.FramesComposer(FrameRecords);

     SaveByteArray(BinDbConverter.GetBinDbData, ReplaceText(CurrentOpenedFile,'.bin','') + '.bin_db');

     BinDbConverter.Done;
  end;
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
       CSVConverter.GetCSVData.SaveToFile('test.csv');
     except
       Application.MessageBox('Unable to save CSV file. Probably it is being used by another process','Error', MB_ICONERROR + MB_OK);
     end;
     CSVConverter.Done;
  end;
end;

procedure TApp.FloatDigitsChange(Sender: TObject);
begin
  NumberEx.Caption:= FloatToStrF(ExampleValue, ffFixed, 10, FloatDigits.Value);
end;

procedure TApp.FormCreate(Sender: TObject);
begin
  TFFVersion:= TFF_V30; // By default
  NumberEx.Caption:= FloatToStrF(ExampleValue, ffFixed, 10, FloatDigits.Value);
end;

procedure TApp.SeparatorCharChange(Sender: TObject);
begin
  if SeparatorChar.Text = '' then SeparatorChar.Text:= ';';
end;

var s: String;

procedure ChangeS(s: String);
begin
  s:= 'ABC';
end;

procedure TApp.TestClick(Sender: TObject);
begin
  s:= '123';
  ShowMessage(s);
  ChangeS(s);
  ShowMessage(s);
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

procedure TApp.S100Change(Sender: TObject);
begin
  if S100.Checked then CSVConverter.SetDateTimeType(3);
  TimeEx.Caption:= '178.125';
end;

procedure TApp.VersionListChange(Sender: TObject);
begin
   TFFVersion:= VersionList.ItemIndex + 2;
end;

end.

