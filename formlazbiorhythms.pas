unit formLazBiorhythms;

{
    A lazarus program to draw a persons Biorhythms
    Copyright (C) <2018>  <Kevin Scott>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
}

{
Curve	Cycle Length
lnSrsPhysical *	23 days
lnSrsEmotional *	28 days
lnSrsIntellectual *	33 days
Spiritual 	53 days
lnSrsAwareness	48 days
lnSrsAesthetic	43 days
lnSrsIntuition	38 days

* Primary curves
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAIntervalSources, TASeries, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, EditBtn, Menus, dateutils,
  formhelp, formLicence, formAbout, formOptions, uOptions;

type

  { TfrmLazBiorhythms }

  TfrmLazBiorhythms = class(TForm)
    btnExit: TButton;
    chckBxPhysical              : TCheckBox;
    chckBxIntellectual          : TCheckBox;
    chckBxEmotional             : TCheckBox;
    chckBxPrimaryCombined       : TCheckBox;
    chckBxSpirtual              : TCheckBox;
    chckBxAesthetic             : TCheckBox;
    chckBxAwareness             : TCheckBox;
    chckBxIntuition             : TCheckBox;
    chckBxSecondaryCombined     : TCheckBox;
    ChrtBiorhythms              : TChart;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    lnSrsAwareness              : TLineSeries;
    lnSrsAesthetic              : TLineSeries;
    lnSrsSecondaryCombined      : TLineSeries;
    lnSrsIntuition              : TLineSeries;
    lnSrsSpiritial              : TLineSeries;
    lnSrsPrimaryCombined        : TLineSeries;
    lnSrsIntellectual           : TLineSeries;
    lnSrsEmotional              : TLineSeries;
    lnSrsPhysical               : TLineSeries;
    lnSrsTodayMark              : TBarSeries;
    GroupBox1                   : TGroupBox;
    GroupBox2                   : TGroupBox;
    GroupBox3                   : TGroupBox;
    lblSecondaryCombined        : TLabel;
    lblIntuition                : TLabel;
    lblAesthetic                : TLabel;
    lblAwareness                : TLabel;
    lblSpirtual                 : TLabel;
    lblCombined                 : TLabel;
    lblEmotional                : TLabel;
    lblIntellectual             : TLabel;
    lblPhysical                 : TLabel;
    lblBirthdayInfo             : TLabel;
    DtEdtBirthDay               : TDateEdit;
    MenuItmOptions: TMenuItem;
    MenuItmExit: TMenuItem;
    MnIimFile: TMenuItem;
    MenuItmHelp: TMenuItem;
    MenuItmLicence: TMenuItem;
    MenuItmAbout: TMenuItem;
    MnItmHelp: TMenuItem;
    MnMenuBio: TMainMenu;
    Panel1                      : TPanel;

    procedure btnExitClick(Sender: TObject);
    procedure DtEdtBirthDayChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure checkBoxClick(Sender: TObject);
    procedure MenuItmAboutClick(Sender: TObject);
    procedure MenuItmExitClick(Sender: TObject);
    procedure MenuItmHelpClick(Sender: TObject);
    procedure MenuItmLicenceClick(Sender: TObject);
    procedure MenuItmOptionsClick(Sender: TObject);
  private
    procedure plotChart;
    procedure clearSeries;
    procedure colourStuff;

  public

  end;

var
  frmLazBiorhythms: TfrmLazBiorhythms;
  userOptions     : Options;              //  used to hold user options.
  birthday        : TDate;
  appStartTime    : int64;                //  used by formAbout to determine how long the app has been running.

implementation

{$R *.lfm}

{ TfrmLazBiorhythms }

procedure TfrmLazBiorhythms.FormCreate(Sender: TObject);
{  Set up stuff at form create.    }
begin
  appStartTime := GetTickCount64;  //  tick count when application starts.

  userOptions  := Options.Create;  // create options file as c:\Users\<user>\AppData\Local\LazBiorhythms\Options.xml

  lblBirthdayInfo.Caption := 'Plesae enter your birthday';

  colourStuff;
end;

procedure TfrmLazBiorhythms.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  userOptions.writeCurrentOptions;  // write out options file.
  userOptions.Free;                 //  Release the user options.
end;

procedure TfrmLazBiorhythms.DtEdtBirthDayChange(Sender: TObject);
{  When a birthdate has been entered, draw chart.    }
begin
  birthday                := DtEdtBirthDay.Date;
  lblBirthdayInfo.Caption := format('You have been alive %d days', [DaysBetween(today, birthday)]);

  plotChart;
end;

procedure TfrmLazBiorhythms.btnExitClick(Sender: TObject);
{  Close the app.    }
begin
  close;
end;

procedure TfrmLazBiorhythms.plotChart;
const
  N = 60;                             //  number of days.
var
  f         : Integer;
  x         : Double;
  min       : integer;
  days      : double;
  sdate     : TdateTime;
  daysAlive : integer;
begin
  clearSeries;

  daysAlive := DaysBetween(today, birthday);

  min   := daysAlive - 30;        //  where to start the play today - 30 days.
  sdate := today;
  sdate := sdate - 30;

  for f:=0 to N-1 do begin
    x := MIN + f;
    days := 2 * PI * x;

    if chckBxPhysical.Checked then
      lnSrsPhysical.AddXY(sdate, sin(days / 23));
    if chckBxEmotional.Checked
      then lnSrsEmotional.AddXY(sdate, sin(days / 28));
    if chckBxIntellectual.Checked
      then lnSrsIntellectual.AddXY(sdate, sin(days / 33));
    if chckBxPrimaryCombined.Checked then
      lnSrsPrimaryCombined.AddXY(sdate, sin(days / 23) + sin(days / 28) + sin(days / 33));

    if chckBxSpirtual.Checked then
      lnSrsSpiritial.AddXY(sdate, sin(days / 53));
    if chckBxAwareness.Checked then
      lnSrsAwareness.AddXY(sdate, sin(days / 48));
    if chckBxAesthetic.Checked then
      lnSrsAesthetic.AddXY(sdate, sin(days / 43));
    if chckBxIntuition.Checked then
      lnSrsIntuition.AddXY(sdate, sin(days / 38));
    if chckBxSecondaryCombined.Checked then
      lnSrsSecondaryCombined.AddXY(sdate, sin(days / 53) + sin(days / 48) + sin(days / 43) + sin(days / 38));

    sdate := sdate + 1;
  end;

  lnSrsTodayMark.AddXY(today,2.5);

end;

procedure TfrmLazBiorhythms.clearSeries;
{  Clears the chart's line series.    }
begin
  lnSrsPhysical.Clear;
  lnSrsEmotional.Clear;
  lnSrsIntellectual.Clear;
  lnSrsPrimaryCombined.Clear;
  lnSrsTodayMark.Clear;

  lnSrsSpiritial.Clear;
  lnSrsAwareness.Clear;
  lnSrsAesthetic.Clear;
  lnSrsIntuition.Clear;
  lnSrsSecondaryCombined.Clear;
end;

procedure TfrmLazBiorhythms.colourStuff;
{  Sets up default colours for the labels on line series.    }
begin

  lnSrsPhysical.SeriesColor := clRed;
  lblPhysical.Font.Color    := clRed;

  lnSrsIntellectual.SeriesColor := clBlue;
  lblIntellectual.Font.Color    := clBlue;

  lnSrsEmotional.SeriesColor := clGreen;
  lblEmotional.Font.Color    := clGreen;

  lnSrsPrimaryCombined.SeriesColor := clAqua;
  lblCombined.Font.Color           := clAqua;

  lnSrsSpiritial.SeriesColor := clMaroon;
  lblSpirtual.Font.Color     := clMaroon;

  lnSrsAwareness.SeriesColor := clFuchsia;
  lblAwareness.Font.Color    := clFuchsia;

  lnSrsAesthetic.SeriesColor := clTeal;
  lblAesthetic.Font.Color    := clTeal;

  lnSrsIntuition.SeriesColor := clSilver;
  lblIntuition.Font.Color    := clSilver;

  lnSrsSecondaryCombined.SeriesColor := clOlive;
  lblSecondaryCombined.Font.Color    := clOlive;
end;

procedure TfrmLazBiorhythms.checkBoxClick(Sender: TObject);
{  A generic event handler called whenever any of the checkboxes are clicked.
   Just calls the plot routine i.e. re-draws if a series is added or removed.
}
begin
  plotChart;
end;
//
//........................................ File Menu ...........................
//
procedure TfrmLazBiorhythms.MenuItmOptionsClick(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TfrmLazBiorhythms.MenuItmExitClick(Sender: TObject);
begin
  close;
end;
//
//........................................ Help Menu ...........................
//
procedure TfrmLazBiorhythms.MenuItmHelpClick(Sender: TObject);
begin
  frmHelp.ShowModal;
end;

procedure TfrmLazBiorhythms.MenuItmLicenceClick(Sender: TObject);
begin
  frmLicence.ShowModal;
end;

procedure TfrmLazBiorhythms.MenuItmAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;
end.

