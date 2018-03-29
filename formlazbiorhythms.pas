unit formLazBiorhythms;

{
Curve	Cycle Length
Physical *	23 days
Emotional *	28 days
Intellectual *	33 days
Spiritual 	53 days
Awareness	48 days
Aesthetic	43 days
Intuition	38 days

* Primary curves
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAIntervalSources, TASeries, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, EditBtn, dateutils;

type

  { TfrmLazBiorhythms }

  TfrmLazBiorhythms = class(TForm)
    ChckGrpPrimaryRhythms: TCheckGroup;
    ChckGrpSecondaryRhythms: TCheckGroup;
    ChrtBiorhythms: TChart;
    awareness: TLineSeries;
    aesthetic: TLineSeries;
    secondaryCombined: TLineSeries;
    intuition: TLineSeries;
    spiritial: TLineSeries;
    todayMark: TBarSeries;
    primaryCombined: TLineSeries;
    intellectual: TLineSeries;
    emotional: TLineSeries;
    physical: TLineSeries;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    DtEdtBirthDay: TDateEdit;
    GroupBox1: TGroupBox;
    lblBirthdayInfo: TLabel;
    Panel1: TPanel;
    procedure ChckGrpPrimaryRhythmsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpSecondaryRhythmsItemClick(Sender: TObject; Index: integer);
    procedure DtEdtBirthDayChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure plotChart;
    procedure clearSeries;
    procedure colourSeriesNames;
  public

  end;

var
  frmLazBiorhythms: TfrmLazBiorhythms;
  birthday        : TDate;
  seriesDrawn     : integer;


implementation

{$R *.lfm}

{ TfrmLazBiorhythms }

procedure TfrmLazBiorhythms.FormCreate(Sender: TObject);
begin
  lblBirthdayInfo.Caption := 'Plesae enter your birthday';
  seriesDrawn := 0;
end;

procedure TfrmLazBiorhythms.DtEdtBirthDayChange(Sender: TObject);
begin
  birthday := DtEdtBirthDay.Date;
  lblBirthdayInfo.Caption := format('You have been alive %d days', [DaysBetween(today, birthday)]);

  plotChart;
end;

procedure TfrmLazBiorhythms.ChckGrpPrimaryRhythmsItemClick(Sender: TObject; Index: integer);
begin
  plotChart;
end;

procedure TfrmLazBiorhythms.ChckGrpSecondaryRhythmsItemClick(Sender: TObject; Index: integer);
begin
  plotChart;
end;

procedure TfrmLazBiorhythms.plotChart;
const
  N = 60;
var
  i: Integer;
  x: Double;
  min: integer;
  days: double;
  sdate: TdateTime;
  daysAlive : integer;
  h : double;
begin
  clearSeries;

  daysAlive := DaysBetween(today, birthday);
  min := daysAlive - 30;
  sdate := today;
  sdate := sdate - 30;
  for i:=0 to N-1 do begin
    x := MIN + i;
    days := 2 * PI * x;

    if ChckGrpPrimaryRhythms.Checked[0] then
      physical.AddXY(sdate, sin(days / 23));
    if ChckGrpPrimaryRhythms.Checked[1] then
      emotional.AddXY(sdate, sin(days / 28));
    if ChckGrpPrimaryRhythms.Checked[2] then
      intellectual.AddXY(sdate, sin(days / 33));
    if ChckGrpPrimaryRhythms.Checked[3] then
      primaryCombined.AddXY(sdate, sin(days / 23) + sin(days / 28) + sin(days / 33));

    if ChckGrpSecondaryRhythms.Checked[0] then
      spiritial.AddXY(sdate, sin(days / 53));
    if ChckGrpSecondaryRhythms.Checked[1] then
      awareness.AddXY(sdate, sin(days / 48));
    if ChckGrpSecondaryRhythms.Checked[2] then
      aesthetic.AddXY(sdate, sin(days / 43));
    if ChckGrpSecondaryRhythms.Checked[3] then
      intuition.AddXY(sdate, sin(days / 38));
    if ChckGrpSecondaryRhythms.Checked[4] then
      secondaryCombined.AddXY(sdate, sin(days / 53) + sin(days / 48) + sin(days / 43) + sin(days / 38));

    sdate := sdate + 1;
  end;

  todayMark.AddXY(today,2.5);

end;

procedure TfrmLazBiorhythms.clearSeries;
begin
  physical.Clear;
  emotional.Clear;
  intellectual.Clear;
  primaryCombined.Clear;
  todayMark.Clear;

  spiritial.Clear;
  awareness.Clear;
  aesthetic.Clear;
  intuition.Clear;
  secondaryCombined.Clear;
end;

procedure TfrmLazBiorhythms.colourSeriesNames;
begin

end;

end.

