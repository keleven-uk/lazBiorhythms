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
    chckBxFirstOverall           : TCheckBox;
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
    chckBxSecondOverall          : TCheckBox;
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
    lnSrsFirstOverall            : TLineSeries;
    lnSrsSecondOverall           : TLineSeries;
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
    lblFirstOverall              : TLabel;
    lblSecondOverall             : TLabel;
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
    btnExit                      : TButton;
    GrpBxBirthDate               : TGroupBox;
    GrpBxFirstUser               : TGroupBox;
    GrpBxSecondUser              : TGroupBox;

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

      if chckBxFirstOverall.Checked then
        lnSrsFirstOverall.AddXY(sdate, sin(days / 23) + sin(days / 28) + sin(days / 33) +
                                       sin(days / 53) + sin(days / 48) + sin(days / 43) + sin(days / 38));
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

      if chckBxSecondOverall.Checked then
        lnSrsSecondOverall.AddXY(sdate, sin(days / 23) + sin(days / 28) + sin(days / 33) +
                                        sin(days / 53) + sin(days / 48) + sin(days / 43) + sin(days / 38));

      sdate := sdate + 1;
    end;
  end;

  setToday;

  DtEdtFirstBirthDay.Date  := userOptions.Firstbirthdate;
  DtEdtSecondBirthDay.Date := userOptions.Secondbirthdate;
end;

procedure TfrmLazBiorhythms.setToday;
{  Drawa a vertical bar to indicate today, on the chart.

   It looks at each user series to try and determine the range.
   The Overall series will be the larger, then the combined.
   The individual series all will have the same magnitude.
   if either Overall or Combined are not used - they return inf, which if ignored.
   All returned values are loaded into a array, then min or max can be determined.
   If none is used, use defaults.
}
VAR
  minArray : array[0..4] of double;
  maxArray : array[0..4] of double;
begin
  minArray[0] := lnSrsFirstSecondaryCombined.MinYValue;
  minArray[1] := lnSrsSecondSecondaryCombined.MinYValue;
  minArray[2] := lnSrsFirstOverall.MinYValue;
  minArray[3] := lnSrsSecondOverall.MinYValue;
  minArray[4] := -2.5;                                  //  use defaults.

  maxArray[0] := lnSrsFirstSecondaryCombined.MaxYValue;
  maxArray[1] := lnSrsSecondSecondaryCombined.MaxYValue;
  maxArray[2] := lnSrsFirstOverall.MaxYValue;
  maxArray[3] := lnSrsSecondOverall.MaxYValue;
  maxArray[4] := 2.5;                                   //  use defaults

  //  draw the verticsal today line.
  lnSrsTodayMark.ZeroLevel := minvalue(minArray);
  lnSrsTodayMark.AddXY(today, maxvalue(maxArray));
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

  lnSrsFirstOverall.Clear;


  lnSrsSecondPhysical.Clear;
  lnSrsSecondEmotional.Clear;
  lnSrsSecondIntellectual.Clear;
  lnSrsSecondPrimaryCombined.Clear;

  lnSrsSecondSpiritial.Clear;
  lnSrsSecondAwareness.Clear;
  lnSrsSecondAesthetic.Clear;
  lnSrsSecondIntuition.Clear;
  lnSrsSecondSecondaryCombined.Clear;

  lnSrsSecondOverall.Clear;


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

  lnSrsFirstPhysical.SeriesColor          := userOptions.clrFirstPhysical;
  lblFirstPhysical.Font.Color             := userOptions.clrFirstPhysical;
  lnSrsFirstIntellectual.SeriesColor      := userOptions.clrFirstIntellectual;
  lblFirstIntellectual.Font.Color         := userOptions.clrFirstIntellectual;
  lnSrsFirstEmotional.SeriesColor         := userOptions.clrFirstEmotional;
  lblFirstEmotional.Font.Color            := userOptions.clrFirstEmotional;
  lnSrsFirstPrimaryCombined.SeriesColor   := userOptions.clrFirstPriCombined;
  lblFirstCombined.Font.Color             := userOptions.clrFirstPriCombined;
  lnSrsFirstSpiritial.SeriesColor         := userOptions.clrFirstSpitual;
  lblFirstSpirtual.Font.Color             := userOptions.clrFirstSpitual;
  lnSrsFirstAwareness.SeriesColor         := userOptions.clrFirstAwareness;
  lblFirstAwareness.Font.Color            := userOptions.clrFirstAwareness;
  lnSrsFirstAesthetic.SeriesColor         := userOptions.clrFirstAesthetic;
  lblFirstAesthetic.Font.Color            := userOptions.clrFirstAesthetic;
  lnSrsFirstIntuition.SeriesColor         := userOptions.clrFirstIntuition;
  lblFirstIntuition.Font.Color            := userOptions.clrFirstIntuition;
  lnSrsFirstSecondaryCombined.SeriesColor := userOptions.clrFirstSecCombined;
  lblFirstSecondaryCombined.Font.Color    := userOptions.clrFirstSecCombined;
  lnSrsFirstOverall.SeriesColor           := userOptions.clrFirstOverall;
  lblFirstOverall.Font.Color              := userOptions.clrFirstOverall;

  lblSecondPhysical.Font.Color             := userOptions.clrSecondPhysical;
  lnSrsSecondPhysical.SeriesColor          := userOptions.clrSecondPhysical;
  lblSecondIntellectual.Font.Color         := userOptions.clrSecondIntellectual;
  lnSrsSecondIntellectual.SeriesColor      := userOptions.clrSecondIntellectual;
  lblSecondEmotional.Font.Color            := userOptions.clrSecondEmotional;
  lnSrsSecondEmotional.SeriesColor         := userOptions.clrSecondEmotional;
  lblSecondCombined.Font.Color             := userOptions.clrSecondPriCombined;
  lnSrsSecondPrimaryCombined.SeriesColor   := userOptions.clrSecondPriCombined;
  lblSecondSpirtual.Font.Color             := userOptions.clrSecondSpitual;
  lnSrsSecondSpiritial.SeriesColor         := userOptions.clrSecondSpitual;
  lblSecondAwareness.Font.Color            := userOptions.clrSecondAwareness;
  lnSrsSecondAwareness.SeriesColor         := userOptions.clrSecondAwareness;
  lblSecondAesthetic.Font.Color            := userOptions.clrSecondAesthetic;
  lnSrsSecondAesthetic.SeriesColor         := userOptions.clrSecondAesthetic;
  lblSecondIntuition.Font.Color            := userOptions.clrSecondIntuition;
  lnSrsSecondIntuition.SeriesColor         := userOptions.clrSecondIntuition;
  lblSecondSecondaryCombined.Font.Color    := userOptions.clrSecondSecCombined;
  lnSrsSecondSecondaryCombined.SeriesColor := userOptions.clrSecondSecCombined;
  lnSrsSecondOverall.SeriesColor           := userOptions.clrSecondOverall;
  lblSecondOverall.Font.Color              := userOptions.clrSecondOverall;
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
  userOptions.useFirstUser     := chckBxFirstUser.Checked;
  GrpBxFirstUser.Enabled       := chckBxFirstUser.Checked;
  lblFirstBirthdayInfo.Enabled := chckBxFirstUser.Checked;
  DtEdtFirstBirthDay.Enabled   := chckBxFirstUser.Checked;

  plotSeries
end;

procedure TfrmLazBiorhythms.chckBxSecondUserChange(Sender: TObject);
begin
  userOptions.useSecondUser     := chckBxSecondUser.Checked;
  GrpBxSecondUser.Enabled       := chckBxSecondUser.Checked;
  lblSecondBirthdayInfo.Enabled := chckBxSecondUser.Checked;
  DtEdtSecondBirthDay.Enabled   := chckBxSecondUser.Checked;
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

