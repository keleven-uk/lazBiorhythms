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
    procedure clrBtnColorChanged(Sender: TObject);
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
  lblFirstSpitual.font.Color          := userOptions.clrFirstSpitual;
  clrBtnFirstSpitual.ButtonColor      := userOptions.clrFirstSpitual;
  lblFirstAesthetic.font.Color        := userOptions.clrFirstAesthetic;
  clrBtnFirstAesthetic.ButtonColor    := userOptions.clrFirstAesthetic;
  lblFirstAwaraness.font.Color        := userOptions.clrFirstAwareness;
  clrBtnFirstAwaraness.ButtonColor    := userOptions.clrFirstAwareness;
  lblFirstIntuition.font.Color        := userOptions.clrFirstIntuition;
  clrBtnFirstIntuition.ButtonColor    := userOptions.clrFirstIntuition;
  lblFirstSecCombined.font.Color      := userOptions.clrFirstSecCombined;
  clrBtnFirstSecCombined.ButtonColor  := userOptions.clrFirstSecCombined;
  lblFirstOverall.Font.Color          := userOptions.clrFirstOverall;
  clrBtnFirstOverall.ButtonColor      := userOptions.clrFirstOverall;

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
{  The Exit button has been pressed, so we forget any changes.
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
  userOptions.writeCurrentOptions;
end;

//
//  ..................................... Colour Buttons .......................
//

procedure TfrmOptions.clrBtnColorChanged(Sender: TObject);
{  This a generic procedure which is call by every colour button when clicked.

   1 A temp colour button is created and assigned to the clicked button [sender].
   2 A label name is generated from the click buttons name.
   3 The labels colour is set.
   4 The appropriate option is set, using a case statement on the label name.
}
VAR
  tmpButton : TColorButton;
  tmpLabel  : TLabel;
  labelName : string;
begin
  //  if not called by a click on a colour button then exit.
  if not (Sender is TColorButton) then Exit;

  //  create a temp colour button and assign to caller.
  tmpButton := TColorButton(Sender);

  //  Determine the label name, will be the same as button name just with a different prefix.
  //  no rfReplaceAll option - just change first occurrence.
  labelName := StringReplace(tmpButton.Name, 'clrBtn', 'lbl', [rfIgnoreCase]);

  //  try and find the label
  tmplabel := FindComponent(labelName) as TLabel;

  if tmpLabel <> nil then                               // found label.
    begin
      tmplabel.Font.Color    := tmpButton.ButtonColor;  //  set label colour to button colour.

      case labelName of
        'lblFirstPhysical'     : userbacOptions.clrFirstPhysical      := tmpButton.ButtonColor;
        'lblFirstIntellectual' : userbacOptions.clrFirstIntellectual  := tmpButton.ButtonColor;
        'lblFirstEmotional'    : userbacOptions.clrFirstEmotional     := tmpButton.ButtonColor;
        'lblFirstPriCombined'  : userbacOptions.clrFirstPriCombined   := tmpButton.ButtonColor;
        'lblFirstSpitual'      : userbacOptions.clrFirstSpitual       := tmpButton.ButtonColor;
        'lblFirstAesthetic'    : userbacOptions.clrFirstAesthetic     := tmpButton.ButtonColor;
        'lblFirstAwaraness'    : userbacOptions.clrFirstAwareness     := tmpButton.ButtonColor;
        'lblFirstIntuition'    : userbacOptions.clrFirstIntuition     := tmpButton.ButtonColor;
        'lblFirstSecCombined'  : userbacOptions.clrFirstSecCombined   := tmpButton.ButtonColor;
        'lblFirstOverall'      : userbacOptions.clrFirstOverall       := tmpButton.ButtonColor;

        'lbllblSecondSpirtual' : userbacOptions.clrSecondSpitual      := tmpButton.ButtonColor;
        'lblSecondAesthetic'   : userbacOptions.clrSecondAesthetic    := tmpButton.ButtonColor;
        'lblSecondAweraness'   : userbacOptions.clrSecondAwareness    := tmpButton.ButtonColor;
        'lblSecondIntuition'   : userbacOptions.clrSecondIntuition    := tmpButton.ButtonColor;
        'lblSecondPriCombined' : userbacOptions.clrSecondPriCombined  := tmpButton.ButtonColor;
        'lblSecondPhysical'    : userbacOptions.clrSecondPhysical     := tmpButton.ButtonColor;
        'lblSecondIntellectual': userbacOptions.clrSecondIntellectual := tmpButton.ButtonColor;
        'lblSecondEmotional'   : userbacOptions.clrSecondEmotional    := tmpButton.ButtonColor;
        'lblSecondSecCombined' : userbacOptions.clrSecondSecCombined  := tmpButton.ButtonColor;
        'lblSecondOverall'     : userbacOptions.clrSecondOverall      := tmpButton.ButtonColor;
      end;  //  case labelName of

    end;    //  if tmpLabel <> nil then

end;


end.

