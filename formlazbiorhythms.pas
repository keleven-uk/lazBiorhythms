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
Cycle           Length
Physical 	23 days *
Emotional 	28 days *
Intellectual	33 days *
Spiritual       53 days
Awareness	48 days
Aesthetic	43 days
Intuition	38 days

* Primary curves
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAIntervalSources, TASeries, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, EditBtn, Menus, dateutils,
  formhelp, formLicence, formAbout, formOptions, uOptions, math;

type

  { TfrmLazBiorhythms }

  TfrmLazBiorhythms = class(TForm)
    chckBxFirstAesthetic         : TCheckBox;
    chckBxSecondAesthetic        : TCheckBox;
    chckBxFirstAwareness         : TCheckBox;
    chckBxSecondAwareness        : TCheckBox;
    chckBxSecondEmotional        : TCheckBox;
    chckBxSecondIntellectual     : TCheckBox;
    chckBxFirstIntuition         : TCheckBox;
    chckBxSecondIntuition        : TCheckBox;
    chckBxSecondPhysical         : TCheckBox;
    chckBxSecondPrimaryCombined  : TCheckBox;
    chckBxFirstSecondaryCombined : TCheckBox;
    chckBxSecondSecondaryCombined: TCheckBox;
    chckBxFirstSpirtual          : TCheckBox;
    chckBxSecondSpirtual         : TCheckBox;
    chckBxFirstPhysical          : TCheckBox;
    chckBxFirstIntellectual      : TCheckBox;
    chckBxFirstEmotional         : TCheckBox;
    chckBxFirstPrimaryCombined   : TCheckBox;
    chckBxSecondUser             : TCheckBox;
    chckBxFirstUser              : TCheckBox;
    ChrtBiorhythms               : TChart;
    DateTimeIntervalChartSource1 : TDateTimeIntervalChartSource;
    lnSrsFirstAwareness          : TLineSeries;
    lnSrsFirstAesthetic          : TLineSeries;
    lnSrsFirstSecondaryCombined  : TLineSeries;
    lnSrsFirstIntuition          : TLineSeries;
    lnSrsFirstSpiritial          : TLineSeries;
    lnSrsFirstPrimaryCombined    : TLineSeries;
    lnSrsFirstIntellectual       : TLineSeries;
    lnSrsFirstEmotional          : TLineSeries;
    lnSrsFirstPhysical           : TLineSeries;
    lnSrsSecondSecondaryCombined : TLineSeries;
    lnSrsSecondAesthetic         : TLineSeries;
    lnSrsSecondIntuition         : TLineSeries;
    lnSrsSecondAwareness         : TLineSeries;
    lnSrsSecondSpiritial         : TLineSeries;
    lnSrsSecondPrimaryCombined   : TLineSeries;
    lnSrsSecondIntellectual      : TLineSeries;
    lnSrsSecondEmotional         : TLineSeries;
    lnSrsSecondPhysical          : TLineSeries;
    lnSrsTodayMark               : TBarSeries;
    lblFirstAesthetic            : TLabel;
    lblSecondAesthetic           : TLabel;
    lblFirstAwareness            : TLabel;
    lblSecondAwareness           : TLabel;
    lblSecondCombined            : TLabel;
    lblSecondEmotional           : TLabel;
    lblSecondIntellectual        : TLabel;
    lblFirstIntuition            : TLabel;
    lblSecondIntuition           : TLabel;
    lblSecondPhysical            : TLabel;
    lblFirstSecondaryCombined    : TLabel;
    lblSecondSecondaryCombined   : TLabel;
    lblFirstSpirtual             : TLabel;
    lblSecondSpirtual            : TLabel;
    btnExit                      : TButton;
    GroupBox1                    : TGroupBox;
    GroupBox2                    : TGroupBox;
    GroupBox3                    : TGroupBox;
    lblFirstCombined             : TLabel;
    lblFirstEmotional            : TLabel;
    lblFirstIntellectual         : TLabel;
    lblFirstPhysical             : TLabel;
    lblFirstBirthdayInfo         : TLabel;
    lblSecondBirthdayInfo        : TLabel;
    DtEdtFirstBirthDay           : TDateEdit;
    DtEdtSecondBirthDay          : TDateEdit;
    MnMenuBio                    : TMainMenu;
    MenuItmOptions               : TMenuItem;
    MenuItmExit                  : TMenuItem;
    MnIimFile                    : TMenuItem;
    MenuItmHelp                  : TMenuItem;
    MenuItmLicence               : TMenuItem;
    MenuItmAbout                 : TMenuItem;
    MnItmHelp                    : TMenuItem;
    Panel1                       : TPanel;

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
    procedure plotChart;
    procedure clearSeries;
    procedure plotSeries;
    procedure colourStuff;
    procedure setToday;

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

  DtEdtFirstBirthDay.Date        := userOptions.Firstbirthdate;
  DtEdtSecondBirthDay.Date       := userOptions.Secondbirthdate;
  DtEdtFirstBirthDay.Font.Color  := clblack;
  DtEdtSecondBirthDay.Font.Color := clblack;

  chckBxFirstUser.Checked  := userOptions.useFirstUser;
  chckBxSecondUser.Checked := userOptions.useSecondUser;
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

procedure TfrmLazBiorhythms.plotChart;
{  Draw the actual Biorhythms.

   first and second user things need a tidy up.
}
const
  N = 60;                             //  number of days.
var
  f         : Integer;
  x         : Double;
  sdays     : integer;
  days      : double;
  sdate     : TdateTime;
  daysAlive : integer;
begin
  clearSeries;
  colourStuff;

  if userOptions.useFirstUser  then
  begin
    daysAlive := DaysBetween(today, userOptions.Firstbirthdate);

    sdays := daysAlive - 30;        //  where to start the plot today - 30 days.
    sdate := today;
    sdate := sdate - 30;

    for f:=0 to N-1 do begin
      x    := sdays + f;
      days := 2 * PI * x;

      if chckBxFirstPhysical.Checked then
        lnSrsFirstPhysical.AddXY(sdate, sin(days / 23));
      if chckBxFirstEmotional.Checked
        then lnSrsFirstEmotional.AddXY(sdate, sin(days / 28));
      if chckBxFirstIntellectual.Checked
        then lnSrsFirstIntellectual.AddXY(sdate, sin(days / 33));
      if chckBxFirstPrimaryCombined.Checked then
        lnSrsFirstPrimaryCombined.AddXY(sdate, sin(days / 23) + sin(days / 28) + sin(days / 33));

      if chckBxFirstSpirtual.Checked then
        lnSrsFirstSpiritial.AddXY(sdate, sin(days / 53));
      if chckBxFirstAwareness.Checked then
        lnSrsFirstAwareness.AddXY(sdate, sin(days / 48));
      if chckBxFirstAesthetic.Checked then
        lnSrsFirstAesthetic.AddXY(sdate, sin(days / 43));
      if chckBxFirstIntuition.Checked then
        lnSrsFirstIntuition.AddXY(sdate, sin(days / 38));
      if chckBxFirstSecondaryCombined.Checked then
        lnSrsFirstSecondaryCombined.AddXY(sdate, sin(days / 53) + sin(days / 48) + sin(days / 43) + sin(days / 38));

      sdate := sdate + 1;
    end;
  end;

  if userOptions.useSecondUser  then
  begin
    daysAlive := DaysBetween(today, userOptions.Secondbirthdate);

    sdays := daysAlive - 30;        //  where to start the plot today - 30 days.
    sdate := today;
    sdate := sdate - 30;

    for f:=0 to N-1 do begin
      x    := sdays + f;
      days := 2 * PI * x;

      if chckBxSecondPhysical.Checked then
        lnSrsSecondPhysical.AddXY(sdate, sin(days / 23));
      if chckBxSecondEmotional.Checked
        then lnSrsSecondEmotional.AddXY(sdate, sin(days / 28));
      if chckBxSecondIntellectual.Checked
        then lnSrsSecondIntellectual.AddXY(sdate, sin(days / 33));
      if chckBxSecondPrimaryCombined.Checked then
        lnSrsSecondPrimaryCombined.AddXY(sdate, sin(days / 23) + sin(days / 28) + sin(days / 33));

      if chckBxSecondSpirtual.Checked then
        lnSrsSecondSpiritial.AddXY(sdate, sin(days / 53));
      if chckBxSecondAwareness.Checked then
        lnSrsSecondAwareness.AddXY(sdate, sin(days / 48));
      if chckBxSecondAesthetic.Checked then
        lnSrsSecondAesthetic.AddXY(sdate, sin(days / 43));
      if chckBxSecondIntuition.Checked then
        lnSrsSecondIntuition.AddXY(sdate, sin(days / 38));
      if chckBxSecondSecondaryCombined.Checked then
        lnSrsSecondSecondaryCombined.AddXY(sdate, sin(days / 53) + sin(days / 48) + sin(days / 43) + sin(days / 38));

      sdate := sdate + 1;
    end;
  end;

  setToday;

  DtEdtFirstBirthDay.Date  := userOptions.Firstbirthdate;
  DtEdtSecondBirthDay.Date := userOptions.Secondbirthdate;
end;

procedure TfrmLazBiorhythms.setToday;
{  Drawa a vertical bar to indicate today, on the chart.

   It look at each user series to try and determine the range.
   The secondary combined (if used) has the greatest range.
   If both user's combines are displayes, use the min and max of the two.
   If neither combined is used, use defaults.
}
VAR
  rangeMin : double;
  rangeMax : double;
begin

  //  use firse user.
  if (lnSrsFirstSecondaryCombined.Count <> 0) and (lnSrsSecondSecondaryCombined.Count = 0) then
  begin
    rangeMin := lnSrsFirstSecondaryCombined.MinYValue;
    rangeMax := lnSrsFirstSecondaryCombined.MaxYValue;
  end;

  //  use second user.
  if (lnSrsFirstSecondaryCombined.Count = 0) and (lnSrsSecondSecondaryCombined.Count <> 0) then
  begin
    rangeMin := lnSrsSecondSecondaryCombined.MinYValue;
    rangeMax := lnSrsSecondSecondaryCombined.MaxYValue;
  end;

  //  use min and max of both users.
  if (lnSrsFirstSecondaryCombined.Count <> 0) and (lnSrsSecondSecondaryCombined.Count <> 0) then
  begin
    rangeMin := min(lnSrsFirstSecondaryCombined.MinYValue, lnSrsSecondSecondaryCombined.MinYValue);
    rangeMax := max(lnSrsFirstSecondaryCombined.MaxYValue, lnSrsSecondSecondaryCombined.MaxYValue);
  end;

  // neither are bing used, set defaults.
  if (lnSrsFirstSecondaryCombined.Count = 0) and (lnSrsSecondSecondaryCombined.Count = 0) then
  begin
    rangeMin := -2.5;
    rangeMax := 2.5;
  end;

  //  draw the verticsal today line.
  lnSrsTodayMark.ZeroLevel := rangeMin;
  lnSrsTodayMark.AddXY(today, rangeMax);
end;

procedure TfrmLazBiorhythms.clearSeries;
{  Clears the chart's line series.    }
begin
  lnSrsFirstPhysical.Clear;
  lnSrsFirstEmotional.Clear;
  lnSrsFirstIntellectual.Clear;
  lnSrsFirstPrimaryCombined.Clear;

  lnSrsFirstSpiritial.Clear;
  lnSrsFirstAwareness.Clear;
  lnSrsFirstAesthetic.Clear;
  lnSrsFirstIntuition.Clear;
  lnSrsFirstSecondaryCombined.Clear;


  lnSrsSecondPhysical.Clear;
  lnSrsSecondEmotional.Clear;
  lnSrsSecondIntellectual.Clear;
  lnSrsSecondPrimaryCombined.Clear;

  lnSrsSecondSpiritial.Clear;
  lnSrsSecondAwareness.Clear;
  lnSrsSecondAesthetic.Clear;
  lnSrsSecondIntuition.Clear;
  lnSrsSecondSecondaryCombined.Clear;


  lnSrsTodayMark.Clear;
end;

procedure TfrmLazBiorhythms.plotSeries;
{  If eisther user is checked then drwa thier curves,
   if neither then clear series.
}
begin
  if (userOptions.useFirstUser or userOptions.useSecondUser) then
    plotChart
  else
    clearSeries;
end;

procedure TfrmLazBiorhythms.colourStuff;
{  Sets up default colours for the labels on line series.    }
begin

  lnSrsFirstPhysical.SeriesColor          := userOptions.clrFirstUserPhysical;
  lblFirstPhysical.Font.Color             := userOptions.clrFirstUserPhysical;
  lnSrsFirstIntellectual.SeriesColor      := userOptions.clrFirstUserIntellectual;
  lblFirstIntellectual.Font.Color         := userOptions.clrFirstUserIntellectual;
  lnSrsFirstEmotional.SeriesColor         := userOptions.clrFirstUserEmotional;
  lblFirstEmotional.Font.Color            := userOptions.clrFirstUserEmotional;
  lnSrsFirstPrimaryCombined.SeriesColor   := userOptions.clrFirstUserPriCombined;
  lblFirstCombined.Font.Color             := userOptions.clrFirstUserPriCombined;
  lnSrsFirstSpiritial.SeriesColor         := userOptions.clrFirstUserSpitual;
  lblFirstSpirtual.Font.Color             := userOptions.clrFirstUserSpitual;
  lnSrsFirstAwareness.SeriesColor         := userOptions.clrFirstUserAwareness;
  lblFirstAwareness.Font.Color            := userOptions.clrFirstUserAwareness;
  lnSrsFirstAesthetic.SeriesColor         := userOptions.clrFirstUserAesthetic;
  lblFirstAesthetic.Font.Color            := userOptions.clrFirstUserAesthetic;
  lnSrsFirstIntuition.SeriesColor         := userOptions.clrFirstUserIntuition;
  lblFirstIntuition.Font.Color            := userOptions.clrFirstUserIntuition;
  lnSrsFirstSecondaryCombined.SeriesColor := userOptions.clrFirstUserSecCombined;
  lblFirstSecondaryCombined.Font.Color    := userOptions.clrFirstUserSecCombined;


  lblSecondPhysical.Font.Color             := userOptions.clrSecondUserPhysical;
  lnSrsSecondPhysical.SeriesColor          := userOptions.clrSecondUserPhysical;
  lblSecondIntellectual.Font.Color         := userOptions.clrSecondUserIntellectual;
  lnSrsSecondIntellectual.SeriesColor      := userOptions.clrSecondUserIntellectual;
  lblSecondEmotional.Font.Color            := userOptions.clrSecondUserEmotional;
  lnSrsSecondEmotional.SeriesColor         := userOptions.clrSecondUserEmotional;
  lblSecondCombined.Font.Color             := userOptions.clrSecondUserPriCombined;
  lnSrsSecondPrimaryCombined.SeriesColor   := userOptions.clrSecondUserPriCombined;
  lblSecondSpirtual.Font.Color             := userOptions.clrSecondUserSpitual;
  lnSrsSecondSpiritial.SeriesColor         := userOptions.clrSecondUserSpitual;
  lblSecondAwareness.Font.Color            := userOptions.clrSecondUserAwareness;
  lnSrsSecondAwareness.SeriesColor         := userOptions.clrSecondUserAwareness;
  lblSecondAesthetic.Font.Color            := userOptions.clrSecondUserAesthetic;
  lnSrsSecondAesthetic.SeriesColor         := userOptions.clrSecondUserAesthetic;
  lblSecondIntuition.Font.Color            := userOptions.clrSecondUserIntuition;
  lnSrsSecondIntuition.SeriesColor         := userOptions.clrSecondUserIntuition;
  lblSecondSecondaryCombined.Font.Color    := userOptions.clrSecondUserSecCombined;
  lnSrsSecondSecondaryCombined.SeriesColor := userOptions.clrSecondUserSecCombined;
end;
//
//........................................ Check Boxes .........................
//
procedure TfrmLazBiorhythms.checkBoxClick(Sender: TObject);
{  A generic event handler called whenever any of the checkboxes are clicked.
   Just calls the plot routine i.e. re-draws if a series is added or removed.
}
begin
  plotSeries
end;

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

