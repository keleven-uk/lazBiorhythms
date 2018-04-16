unit formOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, uOptions;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    btnExit                     : TButton;
    btnOkay                     : TButton;
    btnReset                    : TButton;
    clrBtnSecondUserAesthetic   : TColorButton;
    clrBtnSecondUserAweraness   : TColorButton;
    clrBtnSecondUserEmotional   : TColorButton;
    clrBtnSecondUserIntellectual: TColorButton;
    clrBtnFirstUserPhysical     : TColorButton;
    clrBtnFirstUserIntellectual : TColorButton;
    clrBtnFirstUserEmotional    : TColorButton;
    clrBtnSecondUserPhysical    : TColorButton;
    clrBtnFirstUserSpirtual     : TColorButton;
    clrBtnFirstUserAesthetic    : TColorButton;
    clrBtnFirstUserAweraness    : TColorButton;
    clrBtnSecondUserSpirtual    : TColorButton;
    clrBtnSecSecondUserCombined : TColorButton;
    cltBtnFirstUserIntuition    : TColorButton;
    cltBtnSecondUserIntuition   : TColorButton;
    cltBtnFirstUserPriCombined  : TColorButton;
    clrBtnSecFirstUserCombined  : TColorButton;
    cltBtnSecondUserPriCombined : TColorButton;
    lblSecondUserAesthetic      : TLabel;
    lblFirstUserAweraness       : TLabel;
    lblSecondUserAweraness      : TLabel;
    lblSecondUserEmotional      : TLabel;
    lblSecondUserIntellectual   : TLabel;
    lblFirstUserIntuition       : TLabel;
    lblFirstUserEmotional       : TLabel;
    lblSecondUserIntuition      : TLabel;
    lblSecondUserPhysical       : TLabel;
    lblSecondUserPriCombined    : TLabel;
    lblSecondUserSpirtual       : TLabel;
    lblSecFirstUserCombined     : TLabel;
    lblFirstUserPriCombined     : TLabel;
    lblFirstUserPhysical        : TLabel;
    lblFirstUserIntellectual    : TLabel;
    lblFirstUserSpirtual        : TLabel;
    lblFirstUserAesthetic       : TLabel;
    lblSecSecondUserCombined    : TLabel;
    PageControl1                : TPageControl;
    tbShtGlobal                 : TTabSheet;
    tbShtFirstUsersColous       : TTabSheet;
    tbShtSecondUsersColous      : TTabSheet;
    ColorDialog1                : TColorDialog;
    Panel1                      : TPanel;
    Panel2                      : TPanel;
    Panel3                      : TPanel;
    Panel4                      : TPanel;
    Panel5                      : TPanel;

    procedure btnExitClick(Sender: TObject);
    procedure btnOkayClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
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
  lblFirstUserPhysical.font.Color := userOptions.clrFirstUserPhysical;
  clrBtnFirstUserPhysical.Color   := userOptions.clrFirstUserPhysical;

  lblFirstUserIntellectual.font.Color := userOptions.clrFirstUserIntellectual;
  clrBtnFirstUserIntellectual.Color  := userOptions.clrFirstUserIntellectual;

  lblFirstUserEmotional.font.Color := userOptions.clrFirstUserEmotional;
  clrBtnFirstUserEmotional.Color   := userOptions.clrFirstUserEmotional;

  lblFirstUserPriCombined.font.Color := userOptions.clrFirstUserPriCombined;
  cltBtnFirstUserPriCombined.Color   := userOptions.clrFirstUserPriCombined;

  lblFirstUserSpirtual.font.Color := userOptions.clrFirstUserSpitual;
  clrBtnFirstUserSpirtual.Color  := userOptions.clrFirstUserSpitual;

  lblFirstUserAesthetic.font.Color := userOptions.clrFirstUserAesthetic;
  clrBtnFirstUserAesthetic.Color   := userOptions.clrFirstUserAesthetic;

  lblFirstUserAweraness.font.Color := userOptions.clrFirstUserAwareness;
  clrBtnFirstUserAweraness.Color   := userOptions.clrFirstUserAwareness;

  lblFirstUserIntuition.font.Color := userOptions.clrFirstUserIntuition;
  cltBtnFirstUserIntuition.Color   := userOptions.clrFirstUserIntuition;

  lblSecFirstUserCombined.font.Color := userOptions.clrFirstUserSecCombined;
  clrBtnSecFirstUserCombined.Color   := userOptions.clrFirstUserSecCombined;


  lblSecondUserPhysical.font.Color := userOptions.clrSecondUserPhysical;
  clrBtnSecondUserPhysical.Color   := userOptions.clrSecondUserPhysical;

  lblSecondUserIntellectual.font.Color := userOptions.clrSecondUserIntellectual;
  clrBtnSecondUserIntellectual.Color  := userOptions.clrSecondUserIntellectual;

  lblSecondUserEmotional.font.Color := userOptions.clrSecondUserEmotional;
  clrBtnSecondUserEmotional.Color   := userOptions.clrSecondUserEmotional;

  lblSecondUserPriCombined.font.Color := userOptions.clrSecondUserPriCombined;
  cltBtnSecondUserPriCombined.Color   := userOptions.clrSecondUserPriCombined;

  lblSecondUserSpirtual.font.Color := userOptions.clrSecondUserSpitual;
  clrBtnSecondUserSpirtual.Color  := userOptions.clrSecondUserSpitual;

  lblSecondUserAesthetic.font.Color := userOptions.clrSecondUserAesthetic;
  clrBtnSecondUserAesthetic.Color   := userOptions.clrSecondUserAesthetic;

  lblSecondUserAweraness.font.Color := userOptions.clrSecondUserAwareness;
  clrBtnSecondUserAweraness.Color   := userOptions.clrSecondUserAwareness;

  lblSecondUserIntuition.font.Color := userOptions.clrSecondUserIntuition;
  cltBtnSecondUserIntuition.Color   := userOptions.clrSecondUserIntuition;

  lblSecSecondUserCombined.font.Color := userOptions.clrSecondUserSecCombined;
  clrBtnSecSecondUserCombined.Color   := userOptions.clrSecondUserSecCombined;
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




end.

