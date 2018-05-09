unit formOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, uOptions;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    btnExit                 : TButton;
    btnOkay                 : TButton;
    btnReset                : TButton;
    clrBtnSecondAesthetic   : TColorButton;
    clrBtnSecondAweraness   : TColorButton;
    clrBtnSecondEmotional   : TColorButton;
    clrBtnSecondIntellectual: TColorButton;
    clrBtnFirstPhysical     : TColorButton;
    clrBtnFirstIntellectual : TColorButton;
    clrBtnFirstEmotional    : TColorButton;
    clrBtnSecondPhysical    : TColorButton;
    clrBtnFirstSpitual      : TColorButton;
    clrBtnFirstAesthetic    : TColorButton;
    clrBtnFirstAwaraness    : TColorButton;
    clrBtnSecondSpirtual    : TColorButton;
    clrBtnSecSecondCombined : TColorButton;
    clrBtnFirstIntuition    : TColorButton;
    clrBtnFirstOverall      : TColorButton;
    clrBtnSecondIntuition   : TColorButton;
    clrBtnFirstPriCombined  : TColorButton;
    clrBtnFirstSecCombined  : TColorButton;
    clrBtnSecondPriCombined : TColorButton;
    clrBtnSecondOverall     : TColorButton;
    lblFirstOverall         : TLabel;
    lblSecondAesthetic      : TLabel;
    lblFirstAwaraness       : TLabel;
    lblSecondAweraness      : TLabel;
    lblSecondEmotional      : TLabel;
    lblSecondIntellectual   : TLabel;
    lblFirstIntuition       : TLabel;
    lblFirstEmotional       : TLabel;
    lblSecondIntuition      : TLabel;
    lblSecondPhysical       : TLabel;
    lblSecondPriCombined    : TLabel;
    lblSecondOverall        : TLabel;
    lblSecondSpirtual       : TLabel;
    lblFirstSecCombined     : TLabel;
    lblFirstPriCombined     : TLabel;
    lblFirstPhysical        : TLabel;
    lblFirstIntellectual    : TLabel;
    lblFirstSpitual         : TLabel;
    lblFirstAesthetic       : TLabel;
    lblSecSecondCombined    : TLabel;
    PageControl1            : TPageControl;
    tbShtGlobal             : TTabSheet;
    tbShtFirstUsersColous   : TTabSheet;
    tbShtSecondUsersColous  : TTabSheet;
    ColorDialog1            : TColorDialog;
    Panel1                  : TPanel;
    Panel2                  : TPanel;
    Panel3                  : TPanel;
    Panel4                  : TPanel;
    Panel5                  : TPanel;

    procedure btnExitClick(Sender: TObject);
    procedure btnOkayClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure clrBtnFirstAestheticClick(Sender: TObject);
    procedure clrBtnFirstAwaranessClick(Sender: TObject);
    procedure clrBtnFirstEmotionalClick(Sender: TObject);
    procedure clrBtnFirstIntellectualClick(Sender: TObject);
    procedure clrBtnFirstIntuitionClick(Sender: TObject);
    procedure clrBtnFirstPhysicalClick(Sender: TObject);
    procedure clrBtnFirstPriCombinedClick(Sender: TObject);
    procedure clrBtnFirstOverallClick(Sender: TObject);
    procedure clrBtnFirstSpitualClick(Sender: TObject);
    procedure clrBtnFirstSecCombinedClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure colourStuff;
  public
    { public declarations }
  end; 

var
  frmOptions    : TfrmOptions;
  userBacOptions: Options;          //  holds all the user options.

implementation

uses
  formLazBiorhythms;


{$R *.lfm}

{ TfrmOptions }

//
//  ..................................... Form Events ..........................
//
procedure TfrmOptions.FormActivate(Sender: TObject);
{  Run things when ever the form is shown.    }

var
  optnFile: String;
begin
  //  Create tmp options file as c:\Users\<user>\AppData\Local\<app Name>\OptionsXX_tmp.xml
  //  This is used to store local amendments and only copied to the main options files
  //  if the OK button is clicked.

  {$IFDEF TEST}
    optnFile := 'TEST_Options';
  {$else}
    optnFile := 'Options';
  {$endif}
  {$ifdef WIN32}
    userBacOptions := Options.Create(optnFile + '32_temp.xml');
  {$else}
    userBacOptions := Options.Create(optnFile + '64_temp.xml');
  {$endif}

  userBacOptions.Assign(userOptions);//  make a copy of the current user options

  colourStuff;
end;

procedure TfrmOptions.colourStuff;
begin
  lblFirstPhysical.font.Color         := userOptions.clrFirstPhysical;
  clrBtnFirstPhysical.ButtonColor     := userOptions.clrFirstPhysical;
  lblFirstIntellectual.font.Color     := userOptions.clrFirstIntellectual;
  clrBtnFirstIntellectual.ButtonColor := userOptions.clrFirstIntellectual;
  lblFirstEmotional.font.Color        := userOptions.clrFirstEmotional;
  clrBtnFirstEmotional.ButtonColor    := userOptions.clrFirstEmotional;
  lblFirstPriCombined.font.Color      := userOptions.clrFirstPriCombined;
  clrBtnFirstPriCombined.ButtonColor  := userOptions.clrFirstPriCombined;
  lblFirstSpitual.font.Color         := userOptions.clrFirstSpitual;
  clrBtnFirstSpitual.ButtonColor     := userOptions.clrFirstSpitual;
  lblFirstAesthetic.font.Color        := userOptions.clrFirstAesthetic;
  clrBtnFirstAesthetic.ButtonColor    := userOptions.clrFirstAesthetic;
  lblFirstAwaraness.font.Color        := userOptions.clrFirstAwareness;
  clrBtnFirstAwaraness.ButtonColor    := userOptions.clrFirstAwareness;
  lblFirstIntuition.font.Color        := userOptions.clrFirstIntuition;
  clrBtnFirstIntuition.ButtonColor    := userOptions.clrFirstIntuition;
  lblFirstSecCombined.font.Color      := userOptions.clrFirstSecCombined;
  clrBtnFirstSecCombined.ButtonColor       := userOptions.clrFirstSecCombined;
  lblFirstOverall.Font.Color       := userOptions.clrFirstOverall;
  clrBtnFirstOverall.ButtonColor   := userOptions.clrFirstOverall;

  lblSecondPhysical.font.Color         := userOptions.clrSecondPhysical;
  clrBtnSecondPhysical.ButtonColor     := userOptions.clrSecondPhysical;
  lblSecondIntellectual.font.Color     := userOptions.clrSecondIntellectual;
  clrBtnSecondIntellectual.ButtonColor := userOptions.clrSecondIntellectual;
  lblSecondEmotional.font.Color        := userOptions.clrSecondEmotional;
  clrBtnSecondEmotional.ButtonColor    := userOptions.clrSecondEmotional;
  lblSecondPriCombined.font.Color      := userOptions.clrSecondPriCombined;
  clrBtnSecondPriCombined.ButtonColor  := userOptions.clrSecondPriCombined;
  lblSecondSpirtual.font.Color         := userOptions.clrSecondSpitual;
  clrBtnSecondSpirtual.ButtonColor     := userOptions.clrSecondSpitual;
  lblSecondAesthetic.font.Color        := userOptions.clrSecondAesthetic;
  clrBtnSecondAesthetic.ButtonColor    := userOptions.clrSecondAesthetic;
  lblSecondAweraness.font.Color        := userOptions.clrSecondAwareness;
  clrBtnSecondAweraness.ButtonColor    := userOptions.clrSecondAwareness;
  lblSecondIntuition.font.Color        := userOptions.clrSecondIntuition;
  clrBtnSecondIntuition.ButtonColor    := userOptions.clrSecondIntuition;
  lblSecSecondCombined.font.Color      := userOptions.clrSecondSecCombined;
  clrBtnSecSecondCombined.ButtonColor  := userOptions.clrSecondSecCombined;
  lblSecondOverall.Font.Color          := userOptions.clrSecondOverall;
  clrBtnSecondOverall.ButtonColor      := userOptions.clrSecondOverall;
end;
//
//  ..................................... Forms Buttons ........................
//
procedure TfrmOptions.btnExitClick(Sender: TObject);
{  The Exitl button has been pressed, so we forget any changes.
    Since the changes are made to userBacOptions - we don't need to do nowt.
}
begin
  FreeAndNil(userBacOptions);
  Close;
end;

procedure TfrmOptions.btnOkayClick(Sender: TObject);
{  The OK button has been pressed, Copy the amended changes to main user options.
    Write back the main user options.
}
begin
  userOptions.Assign(userBacOptions);      //  Copy amended options to main user options.
  UserOptions.writeCurrentOptions;         //  write back amended options back to disk.

  FreeAndNil(userBacOptions);
  Close;
end;

procedure TfrmOptions.btnResetClick(Sender: TObject);
{  reset user settings to system default

   NOTE :: maybe should have a confirm dialog.
}
begin
  userOptions.writeDefaultOptions;
end;

//
//  ..................................... Colour Buttons .......................
//
procedure TfrmOptions.clrBtnFirstPhysicalClick(Sender: TObject);
begin
  lblFirstPhysical.Font.Color     := clrBtnFirstPhysical.ButtonColor;
  userbacOptions.clrFirstPhysical := clrBtnFirstPhysical.ButtonColor;
end;

procedure TfrmOptions.clrBtnFirstIntellectualClick(Sender: TObject);
begin
  lblFirstIntellectual.Font.Color     := clrBtnFirstIntellectual.ButtonColor;
  userbacOptions.clrFirstIntellectual := clrBtnFirstIntellectual.ButtonColor;
end;

procedure TfrmOptions.clrBtnFirstEmotionalClick(Sender: TObject);
begin
  lblFirstEmotional.Font.Color     := clrBtnFirstEmotional.ButtonColor;
  userbacOptions.clrFirstEmotional := clrBtnFirstEmotional.ButtonColor;
end;

procedure TfrmOptions.clrBtnFirstPriCombinedClick(Sender: TObject);
begin
  lblFirstPriCombined.Font.Color     := clrBtnFirstPriCombined.ButtonColor;
  userbacOptions.clrFirstPriCombined := clrBtnFirstPriCombined.ButtonColor;
end;

procedure TfrmOptions.clrBtnFirstSpitualClick(Sender: TObject);
begin
  lblFirstSpitual.Font.Color     := clrBtnFirstSpitual.ButtonColor;
  userbacOptions.clrFirstSpitual := clrBtnFirstSpitual.ButtonColor;
end;

procedure TfrmOptions.clrBtnFirstAestheticClick(Sender: TObject);
begin
  lblFirstAesthetic.Font.Color     := clrBtnFirstAesthetic.ButtonColor;
  userbacOptions.clrFirstAesthetic := clrBtnFirstAesthetic.ButtonColor;
end;

procedure TfrmOptions.clrBtnFirstAwaranessClick(Sender: TObject);
begin
  lblFirstAwaraness.Font.Color     := clrBtnFirstAwaraness.ButtonColor;
  userbacOptions.clrFirstAwareness := clrBtnFirstAwaraness.ButtonColor;
end;

procedure TfrmOptions.clrBtnFirstIntuitionClick(Sender: TObject);
begin
  lblFirstIntuition.Font.Color     := clrBtnFirstIntuition.ButtonColor;
  userbacOptions.clrFirstIntuition := clrBtnFirstIntuition.ButtonColor;
end;

procedure TfrmOptions.clrBtnFirstSecCombinedClick(Sender: TObject);
begin
  lblFirstSecCombined.Font.Color     := clrBtnFirstSecCombined.ButtonColor;
  userbacOptions.clrFirstSecCombined := clrBtnFirstSecCombined.ButtonColor;
end;

procedure TfrmOptions.clrBtnFirstOverallClick(Sender: TObject);
begin
  lblFirstOverall.Font.Color     := clrBtnFirstOverall.ButtonColor;
  userbacOptions.clrFirstOverall := clrBtnFirstOverall.ButtonColor;
end;



end.

