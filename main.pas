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
    AddUnits: TCheckBox;
    AddType: TCheckBox;
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
    procedure AddTypeChange(Sender: TObject);
    procedure AddUnitsChange(Sender: TObject);
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

procedure TApp.AddUnitsChange(Sender: TObject);
begin
  TimeHead.Caption:= 'TIME';
  TempHead.Caption:= 'CTRL_Temp';
  if AddUnits.Checked then begin
     TimeHead.Caption:= TimeHead.Caption + '(100S)';
     TempHead.Caption:= TempHead.Caption + '(C)';
  end;
  if AddType.Checked then begin
     TimeHead.Caption:= TimeHead.Caption + '(F4)';
     TempHead.Caption:= TempHead.Caption + '(I1)';
  end;
end;

procedure TApp.AddTypeChange(Sender: TObject);
begin
  TimeHead.Caption:= 'TIME';
  TempHead.Caption:= 'CTRL_Temp';
  if AddUnits.Checked then begin
     TimeHead.Caption:= TimeHead.Caption + '(100S)';
     TempHead.Caption:= TempHead.Caption + '(C)';
  end;
  if AddType.Checked then begin
     TimeHead.Caption:= TimeHead.Caption + '(F4)';
     TempHead.Caption:= TempHead.Caption + '(I1)';
  end;
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
begin
  if LoadSourceFile('bin_db', 100) then begin
     CSVConverter.Init(SeparatorChar.Text[1]);
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

procedure TApp.TestClick(Sender: TObject);
begin
  ShowMessage(IntToStr(GetTypeLegth('U4')));
end;

procedure TApp.UnixTimeChange(Sender: TObject);
begin
  TimeEx.Caption:= '1666337309';
end;

procedure TApp.HumanTimeChange(Sender: TObject);
begin
  TimeEx.Caption:= '31-12-2023 23:55:35';
end;

procedure TApp.S100Change(Sender: TObject);
begin
  TimeEx.Caption:= '178.125';
end;

procedure TApp.VersionListChange(Sender: TObject);
begin
   TFFVersion:= VersionList.ItemIndex + 2;
end;

end.

