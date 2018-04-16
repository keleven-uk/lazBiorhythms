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
lnSrsFirstUserPhysical *	23 days
lnSrsFirstUserEmotional *	28 days
lnSrsFirstUserIntellectual *	33 days
Spiritual 	53 days
lnSrsFirstUserAwareness	48 days
lnSrsFirstUserAesthetic	43 days
lnSrsFirstUserIntuition	38 days

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
    ChrtBiorhythms                 : TChart;
    DateTimeIntervalChartSource1   : TDateTimeIntervalChartSource;
    lnSrsFirstUserAwareness        : TLineSeries;
    lnSrsFirstUserAesthetic        : TLineSeries;
    lnSrsFirstUserSecondaryCombined: TLineSeries;
    lnSrsFirstUserIntuition        : TLineSeries;
    lnFirstUserSrsSpiritial        : TLineSeries;
    lnSrsFirstUserPrimaryCombined  : TLineSeries;
    lnSrsFirstUserIntellectual     : TLineSeries;
    lnSrsFirstUserEmotional        : TLineSeries;
    lnSrsFirstUserPhysical         : TLineSeries;
    lnSrsTodayMark                 : TBarSeries;
    btnExit                        : TButton;
    GroupBox1                      : TGroupBox;
    GroupBox2                      : TGroupBox;
    GroupBox3                      : TGroupBox;
    chckBxSecondUser               : TCheckBox;
    chckBxPhysical                 : TCheckBox;
    chckBxIntellectual             : TCheckBox;
    chckBxEmotional                : TCheckBox;
    chckBxPrimaryCombined          : TCheckBox;
    chckBxSpirtual                 : TCheckBox;
    chckBxAesthetic                : TCheckBox;
    chckBxAwareness                : TCheckBox;
    chckBxIntuition                : TCheckBox;
    chckBxSecondaryCombined        : TCheckBox;
    chckBxFirstUser                : TCheckBox;
    lblFirstUserSecondaryCombined  : TLabel;
    lblFirstUserIntuition          : TLabel;
    lblFirstUserAesthetic          : TLabel;
    lblFirstUserAwareness          : TLabel;
    lblFirstUserSpirtual           : TLabel;
    lblFirstUserCombined           : TLabel;
    lblFirstUserEmotional          : TLabel;
    lblFirstUserIntellectual       : TLabel;
    lblFirstUserPhysical           : TLabel;
    lblFirstBirthdayInfo           : TLabel;
    lblSecondBirthdayInfo          : TLabel;
    DtEdtFirstBirthDay             : TDateEdit;
    DtEdtSecondBirthDay            : TDateEdit;
    MenuItmOptions                 : TMenuItem;
    MenuItmExit                    : TMenuItem;
    MnIimFile                      : TMenuItem;
    MenuItmHelp                    : TMenuItem;
    MenuItmLicence                 : TMenuItem;
    MenuItmAbout                   : TMenuItem;
    MnItmHelp                      : TMenuItem;
    MnMenuBio                      : TMainMenu;
    Panel1                         : TPanel;

    procedure btnExitClick(Sender: TObject);
    procedure chckBxFirstUserChange(Sender: TObject);
    procedure chckBxSecondUserChange(Sender: TObject);
    procedure DtEdtFirstBirthDayChange(Sender: TObject);
    procedure DtEdtSecondBirthDayChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure checkBoxClick(Sender: TObject);
    procedure MenuItmAboutClick(Sender: TObject);
    procedure MenuItmExitClick(Sender: TObject);
    procedure MenuItmHelpClick(Sender: TObject);
    procedure MenuItmLicenceClick(Sender: TObject);
    procedure MenuItmOptionsClick(Sender: TObject);
  private
    procedure plotChart(birthDate: TDate);
    procedure clearSeries;
    procedure plotSeries;
    procedure colourStuff;

  public

  end;

var
  frmLazBiorhythms: TfrmLazBiorhythms;
  userOptions     : Options;              //  used to hold user options.
  appStartTime    : int64;                //  used by formAbout to determine how long the app has been running.

implementation

{$R *.lfm}

{ TfrmLazBiorhythms }

procedure TfrmLazBiorhythms.FormCreate(Sender: TObject);
{  Set up stuff at form create.    }
begin
  appStartTime := GetTickCount64;  //  tick count when application starts.

  userOptions  := Options.Create;  // create options file as c:\Users\<user>\AppData\Local\LazBiorhythms\Options.xml

  lblFirstBirthdayInfo.Caption  := 'Please enter your birthday';
  lblSecondBirthdayInfo.Caption := 'Please enter your birthday';

  DtEdtFirstBirthDay.Date  := userOptions.Firstbirthdate;
  DtEdtSecondBirthDay.Date := userOptions.Secondbirthdate;

  chckBxFirstUser.Checked  := userOptions.useFirstUser;
  chckBxSecondUser.Checked := userOptions.useSecondUser;

  plotSeries
end;

procedure TfrmLazBiorhythms.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  userOptions.writeCurrentOptions;  // write out options file.
  userOptions.Free;                 //  Release the user options.
end;

procedure TfrmLazBiorhythms.DtEdtFirstBirthDayChange(Sender: TObject);
{  When a first birthdate has been entered, draw chart.    }
begin
  userOptions.Firstbirthdate   := DtEdtFirstBirthDay.Date;
  lblFirstBirthdayInfo.Caption := format('You have been alive %d days', [DaysBetween(today, userOptions.Firstbirthdate)]);

  plotSeries
end;

procedure TfrmLazBiorhythms.DtEdtSecondBirthDayChange(Sender: TObject);
{  When a second birthdate has been entered, draw chart.    }
begin
  userOptions.Secondbirthdate   := DtEdtSecondBirthDay.Date;
  lblSecondBirthdayInfo.Caption := format('You have been alive %d days', [DaysBetween(today, userOptions.Secondbirthdate)]);

  plotSeries
end;

procedure TfrmLazBiorhythms.btnExitClick(Sender: TObject);
{  Close the app.    }
begin
  close;
end;

procedure TfrmLazBiorhythms.plotChart(birthDate: TDate);
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
  colourStuff;

  daysAlive := DaysBetween(today, birthDate);

  min   := daysAlive - 30;        //  where to start the play today - 30 days.
  sdate := today;
  sdate := sdate - 30;

  for f:=0 to N-1 do begin
    x := MIN + f;
    days := 2 * PI * x;

    if chckBxPhysical.Checked then
      lnSrsFirstUserPhysical.AddXY(sdate, sin(days / 23));
    if chckBxEmotional.Checked
      then lnSrsFirstUserEmotional.AddXY(sdate, sin(days / 28));
    if chckBxIntellectual.Checked
      then lnSrsFirstUserIntellectual.AddXY(sdate, sin(days / 33));
    if chckBxPrimaryCombined.Checked then
      lnSrsFirstUserPrimaryCombined.AddXY(sdate, sin(days / 23) + sin(days / 28) + sin(days / 33));

    if chckBxSpirtual.Checked then
      lnFirstUserSrsSpiritial.AddXY(sdate, sin(days / 53));
    if chckBxAwareness.Checked then
      lnSrsFirstUserAwareness.AddXY(sdate, sin(days / 48));
    if chckBxAesthetic.Checked then
      lnSrsFirstUserAesthetic.AddXY(sdate, sin(days / 43));
    if chckBxIntuition.Checked then
      lnSrsFirstUserIntuition.AddXY(sdate, sin(days / 38));
    if chckBxSecondaryCombined.Checked then
      lnSrsFirstUserSecondaryCombined.AddXY(sdate, sin(days / 53) + sin(days / 48) + sin(days / 43) + sin(days / 38));

    sdate := sdate + 1;
  end;

  lnSrsTodayMark.AddXY(today,2.5);

  DtEdtFirstBirthDay.Date  := userOptions.Firstbirthdate;
  DtEdtSecondBirthDay.Date := userOptions.Secondbirthdate;
end;

procedure TfrmLazBiorhythms.clearSeries;
{  Clears the chart's line series.    }
begin
  lnSrsFirstUserPhysical.Clear;
  lnSrsFirstUserEmotional.Clear;
  lnSrsFirstUserIntellectual.Clear;
  lnSrsFirstUserPrimaryCombined.Clear;
  lnSrsTodayMark.Clear;

  lnFirstUserSrsSpiritial.Clear;
  lnSrsFirstUserAwareness.Clear;
  lnSrsFirstUserAesthetic.Clear;
  lnSrsFirstUserIntuition.Clear;
  lnSrsFirstUserSecondaryCombined.Clear;
end;

procedure TfrmLazBiorhythms.plotSeries;
begin
  if userOptions.useFirstUser  then
    plotChart(userOptions.Firstbirthdate)
  else if userOptions.useSecondUser then
    plotChart(userOptions.Secondbirthdate)
  else
    clearSeries;
end;

procedure TfrmLazBiorhythms.colourStuff;
{  Sets up default colours for the labels on line series.    }
begin

  lnSrsFirstUserPhysical.SeriesColor := userOptions.clrFirstUserPhysical;
  lblFirstUserPhysical.Font.Color    := userOptions.clrFirstUserPhysical;

  lnSrsFirstUserIntellectual.SeriesColor := userOptions.clrFirstUserIntellectual;
  lblFirstUserIntellectual.Font.Color    := userOptions.clrFirstUserIntellectual;

  lnSrsFirstUserEmotional.SeriesColor := userOptions.clrFirstUserEmotional;
  lblFirstUserEmotional.Font.Color    := userOptions.clrFirstUserEmotional;

  lnSrsFirstUserPrimaryCombined.SeriesColor := userOptions.clrFirstUserPriCombined;
  lblFirstUserCombined.Font.Color           := userOptions.clrFirstUserPriCombined;

  lnFirstUserSrsSpiritial.SeriesColor := userOptions.clrFirstUserSpitual;
  lblFirstUserSpirtual.Font.Color     := userOptions.clrFirstUserSpitual;

  lnSrsFirstUserAwareness.SeriesColor := userOptions.clrFirstUserAwareness;
  lblFirstUserAwareness.Font.Color    := userOptions.clrFirstUserAwareness;

  lnSrsFirstUserAesthetic.SeriesColor := userOptions.clrFirstUserAesthetic;
  lblFirstUserAesthetic.Font.Color    := userOptions.clrFirstUserAesthetic;

  lnSrsFirstUserIntuition.SeriesColor := userOptions.clrFirstUserIntuition;
  lblFirstUserIntuition.Font.Color    := userOptions.clrFirstUserIntuition;

  lnSrsFirstUserSecondaryCombined.SeriesColor := userOptions.clrFirstUserSecCombined;
  lblFirstUserSecondaryCombined.Font.Color    := userOptions.clrFirstUserSecCombined;
end;

procedure TfrmLazBiorhythms.checkBoxClick(Sender: TObject);
{  A generic event handler called whenever any of the checkboxes are clicked.
   Just calls the plot routine i.e. re-draws if a series is added or removed.
}
begin
  plotSeries
end;
//
//........................................ Check Boxes .........................
//
procedure TfrmLazBiorhythms.chckBxFirstUserChange(Sender: TObject);
begin
  userOptions.useFirstUser := chckBxFirstUser.Checked;

  plotSeries
end;

procedure TfrmLazBiorhythms.chckBxSecondUserChange(Sender: TObject);
begin
  userOptions.useSecondUser := chckBxSecondUser.Checked;

  plotSeries
end;
//
//........................................ File Menu ...........................
//
procedure TfrmLazBiorhythms.MenuItmOptionsClick(Sender: TObject);
begin
  frmOptions.ShowModal;

  //  in case something has changed.
  colourStuff;
  plotSeries
end;

procedure TfrmLazBiorhythms.MenuItmExitClick(Sender: TObject);
begin
  userOptions.writeCurrentOptions;
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

