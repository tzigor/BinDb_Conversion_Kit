unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, DateUtils,
  StrUtils, Utils, UserTypes, ParseBin, TffObjects, BIN_DB_Converter, CSV_Converter;

type

  { TApp }

  TApp = class(TForm)
    ConvertToCSV: TButton;
    Test: TButton;
    Label1: TLabel;
    VersionList: TComboBox;
    ConvertFile: TButton;
    CloseApp: TButton;
    FileName: TEdit;
    OpenDialog: TOpenDialog;
    OpenFile: TButton;
    PageControl1: TPageControl;
    MainTab: TTabSheet;
    procedure CloseAppClick(Sender: TObject);
    procedure ConvertFileClick(Sender: TObject);
    procedure ConvertToCSVClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenFileClick(Sender: TObject);
    procedure TestClick(Sender: TObject);
    procedure VersionListChange(Sender: TObject);
  private

  public

  end;

var
  App: TApp;

  TFFVersion         : Byte;
  CurrentOpenedFile  : String;
  Bytes              : TBytes;   // Data source
  TffStructure       : TTffStructure;
  FrameRecords       : TFrameRecords;
  BinDbConverter     : TBinDbConverter;

implementation

{$R *.lfm}

{ TApp }

procedure TApp.CloseAppClick(Sender: TObject);
begin
  App.Close;
end;

procedure TApp.ConvertFileClick(Sender: TObject);
begin
  if CurrentOpenedFile <> '' then begin
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

end;

procedure TApp.FormCreate(Sender: TObject);
begin
  TFFVersion:= TFF_V30 // By default
end;

procedure TApp.OpenFileClick(Sender: TObject);
begin
  if LoadBinFile then FileName.Text:= CurrentOpenedFile;
end;

procedure TApp.TestClick(Sender: TObject);
begin
  ShowMessage(IntToStr(GetTypeLegth('U4')));
end;

procedure TApp.VersionListChange(Sender: TObject);
begin
   TFFVersion:= VersionList.ItemIndex + 2;
end;

end.

