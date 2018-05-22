unit uOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite, XMLRead, fileinfo, winpeimagereader, Dialogs,
  Graphics, LazBiorhythmsUntils;

type
  Options = class
  {  Holds the current user options.  The options are hard coded in this class.

     When the class is created, the constructor first checks that the user directory
     exists - this should be something like c:\Users\<user>\AppData\Local\<app Name>\Options.xml.
     The file name can be passed to the contructor, if it's absent the 'Options.xml' is used.
     If this directory does not exist, it will be created.

     Next, the user options file is checked.  If it is exists, it is read.
     If the file does not exist - the file will be created with default values.

     class methods -
         create                       - creates the options class with a default filename.
         create(fileName: string)     - creates the options class with a specified filename.
         readOptions                  - reads the options file and populates the options class
         writeCurrentOptions          - writes the current options to the options filename [in XML].
         writeDefaultOptions          - writes the options file with default files [internal use really]
                                      - could be used to reset options.

     The writeCurrentOptions should be executed when the program closes, or when options change.
     That is if the changes need to be saved.

     NOTE :: If there is an error while either reading or writing the options file, the application is halted.

     NOTE :: All XML values are string, so need to be casted before use - this is done in the read / write routines.

  }
    private
      _dirName: String;
      //  Global
      _Comments        : string;
      _companyName     : string;
      _fileDescription : string;
      _fileVersion     : string;
      _InternalName    : string;
      _legalCopyright  : string;
      _originalFileName: string;
      _productName     : string;
      _productVersion  : string;

      _optionsName    : string;             //  full path to the options file.
      _Firstbirthdate : TDateTime;          //  Birthday of First user.
      _Secondbirthdate: TDateTime;          //  Birthday of Second user.
      _useFirstUser   : boolean;
      _useSecondUser  : boolean;

      //  Colours
      _clrFirstPhysical    : TColor;
      _clrFirstIntellectual: TColor;
      _clrFirstEmotional   : TColor;
      _clrFirstPriCombined : TColor;
      _clrFirstSpitual     : TColor;
      _clrFirstAesthetic   : TColor;
      _clrFirstAwareness   : TColor;
      _clrFirstIntuition   : TColor;
      _clrFirstSecCombined : TColor;
      _clrFirstOverall     : TColor;

      _clrSecondPhysical    : TColor;
      _clrSecondIntellectual: TColor;
      _clrSecondEmotional   : TColor;
      _clrSecondPriCombined : TColor;
      _clrSecondSpitual     : TColor;
      _clrSecondAesthetic   : TColor;
      _clrSecondAwareness   : TColor;
      _clrSecondIntuition   : TColor;
      _clrSecondSecCombined : TColor;
      _clrSecondOverall     : TColor;

      procedure checkDirectory;
      Function readChild(PassNode: TDOMNode;  name: string): string;
      Function readChildAttribute(PassNode: TDOMNode;  name: string; attribute: string): string;
      function writeStrChild(Doc: TXMLDocument; name: string; value: string): TDOMNode;
      function writeFontChild(Doc: TXMLDocument; name: string; value: Tfont): TDOMNode;
      function writeDateChild(Doc: TXMLDocument; name: string; value: TDateTime): TDOMNode;
      function writeColChild(Doc: TXMLDocument; name: string; value: TColor): TDOMNode;
      function writeBolChild(Doc: TXMLDocument; name: string; value: boolean): TDOMNode;
      function writeIntChild(Doc: TXMLDocument; name: string; value: integer): TDOMNode;
      function writeFloatChild(Doc: TXMLDocument; name: string; value: Double): TDOMNode;
      function writeIntChildAttribute(Doc: TXMLDocument; name: string; value1: integer; value2: integer): TDOMNode;
    public
      //  Global - file stuff
      property Comments        : String read _Comments         write _Comments;
      property companyName     : String read _companyName      write _companyName;
      property fileDescription : String read _fileDescription  write _fileDescription;
      property fileVersion     : String read _fileVersion      write _fileVersion;
      property InternalName    : String read _InternalName     write _InternalName;
      property legalCopyright  : String read _legalCopyright   write _legalCopyright;
      property originalFileName: String read _originalFileName write _originalFileName;
      property productName     : String read _productName      write _productName;
      property productVersion  : String read _productVersion   write _productVersion;

      //  Global - other stuff
      property optionsName     : string    read _optionsName     write _optionsName;
      property Firstbirthdate  : TDateTime read _Firstbirthdate  write _Firstbirthdate;
      property Secondbirthdate : TDateTime read _Secondbirthdate write _Secondbirthdate;
      property useFirstUser    : boolean   read _useFirstUser    write _useFirstUser;
      property useSecondUser   : boolean   read _useSecondUser   write _useSecondUser;

      //  Colours
      property clrFirstPhysical    : TColor read _clrFirstPhysical     write _clrFirstPhysical;
      property clrFirstIntellectual: TColor read _clrFirstIntellectual write _clrFirstIntellectual;
      property clrFirstEmotional   : TColor read _clrFirstEmotional    write _clrFirstEmotional;
      property clrFirstPriCombined : TColor read _clrFirstPriCombined  write _clrFirstPriCombined;
      property clrFirstSpitual     : TColor read _clrFirstSpitual      write _clrFirstSpitual;
      property clrFirstAesthetic   : TColor read _clrFirstAesthetic    write _clrFirstAesthetic;
      property clrFirstAwareness   : TColor read _clrFirstAwareness    write _clrFirstAwareness;
      property clrFirstIntuition   : TColor read _clrFirstIntuition    write _clrFirstIntuition;
      property clrFirstSecCombined : TColor read _clrFirstSecCombined  write _clrFirstSecCombined;
      property clrFirstOverall     : TColor read _clrFirstOverall      write _clrFirstOverall;

      property clrSecondPhysical    : TColor read _clrSecondPhysical     write _clrSecondPhysical;
      property clrSecondIntellectual: TColor read _clrSecondIntellectual write _clrSecondIntellectual;
      property clrSecondEmotional   : TColor read _clrSecondEmotional    write _clrSecondEmotional;
      property clrSecondPriCombined : TColor read _clrSecondPriCombined  write _clrSecondPriCombined;
      property clrSecondSpitual     : TColor read _clrSecondSpitual      write _clrSecondSpitual;
      property clrSecondAesthetic   : TColor read _clrSecondAesthetic    write _clrSecondAesthetic;
      property clrSecondAwareness   : TColor read _clrSecondAwareness    write _clrSecondAwareness;
      property clrSecondIntuition   : TColor read _clrSecondIntuition    write _clrSecondIntuition;
      property clrSecondSecCombined : TColor read _clrSecondSecCombined  write _clrSecondSecCombined;
      property clrSecondOverall     : TColor read _clrSecondOverall      write _clrSecondOverall;

      constructor Create; overload;
      constructor Create(filename: String); overload;

      procedure readOptions;
      procedure Assign(o: Options);
      procedure writeCurrentOptions;
      procedure writeDefaultOptions;
  end;  //  class

  myFileVersionInfo = class
  {  Retrieves the current file info.

     see http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company.

     NOTE :: The information in Project / Project Options / Version Ino in the IDE
     must be up to date.
  }

    private
      _comments        : String;
      _companyName     : String;
      _fileDescription : String;
      _fileVersion     : String;
      _InternalName    : String;
      _legalCopyright  : String;
      _originalFileName: String;
      _productName     : String;
      _productVersion  : String;

    public
      property fileComments: String         read _comments         write _comments;
      property fileCompanyName: String      read _companyName      write _companyName;
      property fileFileDescription: String  read _fileDescription  write _fileDescription;
      property fileFileVersion: String      read _fileVersion      write _fileVersion;
      property fileInternalName: String     read _InternalName     write _InternalName;
      property fileLegalCopyright: String   read _legalCopyright   write _legalCopyright;
      property fileOriginalFileName: String read _originalFileName write _originalFileName;
      property fileProductName: String      read _productName      write _productName;
      property fileProductVersion: String   read _productVersion   write _productVersion;

      procedure GetFileInfo;
  end;

implementation

//
//............................................ Options methods ............................................
//

  constructor Options.Create; overload;
  {  creates the options class with a default filename.  }
  var
  optnFile: String;
  begin
    checkDirectory;

    {$IFDEF TEST}
      optnFile := 'TEST_Options';
    {$else}
      optnFile := 'Options';
    {$endif}
    {$ifdef WIN32}
      optionsName := _dirName + optnFile + '32.xml';
    {$else}
      optionsName := _dirName + optnFile + '64.xml';
    {$endif}

    If FileExists(optionsName) Then
    begin
      writeDefaultOptions;       //  Set up the defaults [which includes any new values]
      readOptions;               //  read the exiting options file.
      writeCurrentOptions;       //  write out a new options file [which includes any new values]
    end
    else
      writeDefaultOptions;       //  options file not found, write out a new one.
  end;

  constructor Options.Create(fileName: String); overload;
  {  creates the options class with a specified filename.  }
  begin
    checkDirectory;

    optionsName := _dirName + fileName;

    If FileExists(optionsName) Then
    begin
      writeDefaultOptions;       //  Set up the defaults [which includes any new values]
      readOptions;               //  read the exiting options file.
      writeCurrentOptions;       //  write out a new options file [which includes any new values]
    end
    else
      writeDefaultOptions;       //  options file not found, write out a new one.
  end;

  procedure Options.checkDirectory;
  {  Checks that the options directory exists.

     GetAppConfigDir(False) -> c:\Users\<user>\AppData\Local\<app Name>\
     GetAppConfigDir(True)  -> c:\ProgramData\<app Name>\
  }
  begin
    _dirName := GetAppConfigDir(False);

    If NOT DirectoryExists(_dirName) Then
      If Not CreateDir (_dirName) Then
        ShowMessage('Failed to create directory !');
  end;

  procedure Options.Assign(o: Options);
  {  Copy all fields from one options object to another.
   Because Options is derived from TObjects and not TPersistent, we don't get assign for free.

   NOTE :: When a new field of added to the Option class, it HAS to be added here.
           Must be a better way of doing this.
  }
  begin
  //  Global - file stuff
  Comments         := o.Comments;
  companyName      := o.companyName;
  fileDescription  := o.fileDescription;
  fileVersion      := o.fileVersion;
  InternalName     := o.InternalName;
  legalCopyright   := o.legalCopyright;
  originalFileName := o.originalFileName;
  productName      := o.productName;
  productVersion   := o.productVersion;

  //  Global - other stuff
  optionsName     := o.optionsName;
  Firstbirthdate  := o.Firstbirthdate;
  Secondbirthdate := o.Secondbirthdate;
  useFirstUser    := o.useFirstUser;
  useSecondUser   := o.useSecondUser;

  //  Colours
  _clrFirstPhysical     := o.clrFirstPhysical;
  _clrFirstIntellectual := o.clrFirstIntellectual;
  _clrFirstEmotional    := o.clrFirstEmotional;
  _clrFirstPriCombined  := o.clrFirstPriCombined;
  _clrFirstSpitual      := o.clrFirstSpitual;
  _clrFirstAesthetic    := o.clrFirstAesthetic;
  _clrFirstAwareness    := o.clrFirstAwareness;
  _clrFirstIntuition    := o.clrFirstIntuition;
  _clrFirstSecCombined  := o.clrFirstSecCombined;
  _clrFirstOverall      := o.clrFirstOverall;

  _clrSecondPhysical     := o.clrSecondPhysical;
  _clrSecondIntellectual := o.clrSecondIntellectual;
  _clrSecondEmotional    := o.clrSecondEmotional;
  _clrSecondPriCombined  := o.clrSecondPriCombined;
  _clrSecondSpitual      := o.clrSecondSpitual;
  _clrSecondAesthetic    := o.clrSecondAesthetic;
  _clrSecondAwareness    := o.clrSecondAwareness;
  _clrSecondIntuition    := o.clrSecondIntuition;
  _clrSecondSecCombined  := o.clrSecondSecCombined;
  _clrSecondOverall      := o.clrSecondOverall;

  end;

  procedure Options.readOptions;
  {  Read in the options file.
   The filename is specified when the user options class is created.
   The file info is re-read, in case is has changed.

   The read is now a two stage process.
   Stage 1 - the xml node is read, this return a string value.
   stage 2 - if the return is 'ERROR', the xml node is missing and then use the
             default value.  If not the return will hold the value which is passed
             to the options property.

   NOTE : This cures the missing child problem, BUT NOT the missing node.
  }
  var
    fvi      : myFileVersionInfo;
    PassNode : TDOMNode;
    childNode: TDOMNode;
    Doc      : TXMLDocument;
    rtn      : string;
  begin
    try
      //  retrieve file info i.e build numner etc.
      fvi:= myFileVersionInfo.create;
      fvi.GetFileInfo;

      Comments := fvi.fileComments;
      companyName := fvi.fileCompanyName;
      fileDescription := fvi.fileFileDescription;
      fileVersion := fvi.fileFileVersion;
      InternalName := fvi.fileInternalName;
      legalCopyright := fvi.fileLegalCopyright;
      originalFileName := fvi.fileOriginalFileName;
      productName := fvi.fileProductName;
      productVersion := fvi.fileProductVersion;

      try
        // Read in xml file from disk
        ReadXMLFile(Doc, optionsName);
      except
        on E: Exception do
        begin
          ShowMessage('ERROR: reading XML file.' + LineEnding
                    + E.Message + LineEnding
                    + 'Halting Program Execution');
          Halt;
        end;  //  on E:
      end;    //  try

      //  Global
      PassNode := Doc.DocumentElement.FindNode('Global');

      if assigned(PassNode) then
      begin
        rtn := readChild(PassNode, 'optionsName');
        if rtn <> 'ERROR' then optionsName := ansistring(rtn);
        rtn := readChild(PassNode, 'Firstbirthdate');
        if rtn <> 'ERROR' then Firstbirthdate := strToDate(rtn);
        rtn := readChild(PassNode, 'Secondbirthdate');
        if rtn <> 'ERROR' then Secondbirthdate := strToDate(rtn);
        rtn := readChild(PassNode, 'useFirstUser');
        if rtn <> 'ERROR' then useFirstUser := strToBool(rtn);
        rtn := readChild(PassNode, 'useSecondUser');
        if rtn <> 'ERROR' then useSecondUser := strToBool(rtn);
      end;

      //  Colours
      PassNode := Doc.DocumentElement.FindNode('Colours');

      if assigned(PassNode) then
      begin
        rtn := readChild(PassNode, 'clrFirstPhysical');
        if rtn <> 'ERROR' then clrFirstPhysical := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrFirstIntellectual');
        if rtn <> 'ERROR' then clrFirstIntellectual := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrFirstEmotional');
        if rtn <> 'ERROR' then clrFirstEmotional := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrFirstPriCombined');
        if rtn <> 'ERROR' then clrFirstPriCombined := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrFirstSpitual');
        if rtn <> 'ERROR' then clrFirstSpitual := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrFirstAesthetic');
        if rtn <> 'ERROR' then clrFirstAesthetic := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrFirstAwareness');
        if rtn <> 'ERROR' then clrFirstAwareness := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrFirstIntuition');
        if rtn <> 'ERROR' then clrFirstIntuition := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrFirstSecCombined');
        if rtn <> 'ERROR' then clrFirstSecCombined := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrFirstOverall');
        if rtn <> 'ERROR' then clrFirstOverall := StringToColor(rtn);

        rtn := readChild(PassNode, 'clrSecondPhysical');
        if rtn <> 'ERROR' then clrSecondPhysical := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrSecondIntellectual');
        if rtn <> 'ERROR' then clrSecondIntellectual := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrSecondEmotional');
        if rtn <> 'ERROR' then clrSecondEmotional := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrSecondPriCombined');
        if rtn <> 'ERROR' then clrSecondPriCombined := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrSecondSpitual');
        if rtn <> 'ERROR' then clrSecondSpitual := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrSecondAesthetic');
        if rtn <> 'ERROR' then clrSecondAesthetic := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrSecondAwareness');
        if rtn <> 'ERROR' then clrSecondAwareness := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrSecondIntuition');
        if rtn <> 'ERROR' then clrSecondIntuition := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrSecondSecCombined');
        if rtn <> 'ERROR' then clrSecondSecCombined := StringToColor(rtn);
        rtn := readChild(PassNode, 'clrSecondOverall');
        if rtn <> 'ERROR' then clrSecondOverall := StringToColor(rtn);
      end;

    finally
      // finally, free the document
      Doc.Free;
    end;
  end;

  procedure Options.writeDefaultOptions;
  {  Sets us some sensible defaults and then calls writeCurrentOptions to writs out the xml file.
     Used if the useroptions file does not exist.
     The filename is specified when the user options class is created.
     The file info is re-read, in case is has changed
  }
  var
  fvi: myFileVersionInfo;
  begin
    //  retrieve file info i.e build numner etc.
    fvi:= myFileVersionInfo.create;
    fvi.GetFileInfo;

    Comments         := fvi.fileComments ;
    companyName      := fvi.fileCompanyName;
    fileDescription  := fvi.fileFileDescription;
    fileVersion      := fvi.fileFileVersion;
    InternalName     := fvi.fileInternalName;
    legalCopyright   := fvi.fileLegalCopyright;
    originalFileName := fvi.fileOriginalFileName;
    productName      := fvi.fileProductName;
    productVersion   := fvi.fileProductVersion;

    // Global
    Firstbirthdate  := encodeDate(1958, 04, 02);
    Secondbirthdate := encodeDate(1960, 10, 11);
    useFirstUser    := true;
    useSecondUser   := true;

    //  Colours
    clrFirstPhysical     := clRed;
    clrFirstIntellectual := clBlue;
    clrFirstEmotional    := clGreen;
    clrFirstPriCombined  := clAqua;
    clrFirstSpitual      := clMaroon;
    clrFirstAesthetic    := clTeal;
    clrFirstAwareness    := clFuchsia;
    clrFirstIntuition    := clSilver;
    clrFirstSecCombined  := clOlive;
    clrFirstOverall      := clLime;

    clrSecondPhysical     := clRed;
    clrSecondIntellectual := clBlue;
    clrSecondEmotional    := clGreen;
    clrSecondPriCombined  := clAqua;
    clrSecondSpitual      := clMaroon;
    clrSecondAesthetic    := clTeal;
    clrSecondAwareness    := clFuchsia;
    clrSecondIntuition    := clSilver;
    clrSecondSecCombined  := clOlive;
    clrSecondOverall      := clLime;
  end;

  procedure Options.writeCurrentOptions;
  {  Writes out the user options to a xml file.
     The filename is specified when the user options class is created.
     The file info is re-read, in case is has changed
  }
  var
  Doc        : TXMLDocument;
  RootNode   : TDOMNode;
  ElementNode: TDOMNode;
  ItemNode   : TDOMNode;
  TextNode   : TDOMNode;
  fvi        : myFileVersionInfo;
  begin
    try
      //  retrieve file info i.e build numner etc.
      fvi:= myFileVersionInfo.create;
      fvi.GetFileInfo;

      // Create a document
      Doc := TXMLDocument.Create;

      // Create a root node
      RootNode := Doc.CreateElement('LazBiorhythms');
      Doc.Appendchild(RootNode);
      RootNode:= Doc.DocumentElement;

      //  Global
      ElementNode:=Doc.CreateElement('Global');

      ElementNode.AppendChild(writeStrChild(doc, 'Comments',         fvi.fileComments));
      ElementNode.AppendChild(writeStrChild(doc, 'companyName',      fvi.filecompanyName));
      ElementNode.AppendChild(writeStrChild(doc, 'fileDescription',  fvi.filefileDescription));
      ElementNode.AppendChild(writeStrChild(doc, 'fileVersion',      fvi.filefileVersion));
      ElementNode.AppendChild(writeStrChild(doc, 'InternalName',     fvi.fileInternalName));
      ElementNode.AppendChild(writeStrChild(doc, 'legalCopyright',   fvi.fileLegalCopyright));
      ElementNode.AppendChild(writeStrChild(doc, 'originalFileName', fvi.fileOriginalFileName));
      ElementNode.AppendChild(writeStrChild(doc, 'productName',      fvi.fileProductName));
      ElementNode.AppendChild(writeStrChild(doc, 'productVersion',   fvi.fileProductVersion));

      ElementNode.AppendChild(writeStrChild(doc,  'optionsName',     optionsName));
      ElementNode.AppendChild(writeDateChild(doc, 'Firstbirthdate',  Firstbirthdate));
      ElementNode.AppendChild(writeDateChild(doc, 'Secondbirthdate', Secondbirthdate));
      ElementNode.AppendChild(writeBolChild(doc,  'useFirstUser',    useFirstUser));
      ElementNode.AppendChild(writeBolChild(doc,  'useSecondUser',   useSecondUser));

      RootNode.AppendChild(ElementNode);

      //Colours
      ElementNode := Doc.CreateElement('Colours');

      ElementNode.AppendChild(writeColChild(doc, 'clrFirstPhysical',     clrFirstPhysical));
      ElementNode.AppendChild(writeColChild(doc, 'clrFirstIntellectual', clrFirstIntellectual));
      ElementNode.AppendChild(writeColChild(doc, 'clrFirstEmotional',    clrFirstEmotional));
      ElementNode.AppendChild(writeColChild(doc, 'clrFirstPriCombined',  clrFirstPriCombined));
      ElementNode.AppendChild(writeColChild(doc, 'clrFirstSpitual',      clrFirstSpitual));
      ElementNode.AppendChild(writeColChild(doc, 'clrFirstAesthetic',    clrFirstAesthetic));
      ElementNode.AppendChild(writeColChild(doc, 'clrFirstAwareness',    clrFirstAwareness));
      ElementNode.AppendChild(writeColChild(doc, 'clrFirstIntuition',    clrFirstIntuition));
      ElementNode.AppendChild(writeColChild(doc, 'clrFirstSecCombined',  clrFirstSecCombined));
      ElementNode.AppendChild(writeColChild(doc, 'clrFirstOverall',      clrFirstOverall));

      ElementNode.AppendChild(writeColChild(doc, 'clrSecondPhysical',     clrSecondPhysical));
      ElementNode.AppendChild(writeColChild(doc, 'clrSecondIntellectual', clrSecondIntellectual));
      ElementNode.AppendChild(writeColChild(doc, 'clrSecondEmotional',    clrSecondEmotional));
      ElementNode.AppendChild(writeColChild(doc, 'clrSecondPriCombined',  clrSecondPriCombined));
      ElementNode.AppendChild(writeColChild(doc, 'clrSecondSpitual',      clrSecondSpitual));
      ElementNode.AppendChild(writeColChild(doc, 'clrSecondAesthetic',    clrSecondAesthetic));
      ElementNode.AppendChild(writeColChild(doc, 'clrSecondAwareness',    clrSecondAwareness));
      ElementNode.AppendChild(writeColChild(doc, 'clrSecondIntuition',    clrSecondIntuition));
      ElementNode.AppendChild(writeColChild(doc, 'clrSecondSecCombined',  clrSecondSecCombined));
      ElementNode.AppendChild(writeColChild(doc, 'clrSecondOverall',      clrSecondOverall));

      RootNode.AppendChild(ElementNode);

      try
        // Save XML
        WriteXMLFile(Doc, optionsName);
      except
        on E: Exception do
        begin
          ShowMessage('ERROR: Writing XML file.' + LineEnding
                    + E.Message + LineEnding
                    + 'Halting Program Execution');
          Halt;
        end;  //  on E:
      end;    //  try

    finally
      Doc.Free;
    end;
  end;

//
//........................................ fileVersionInfo methods ............................................
//

  procedure myFileVersionInfo.GetFileInfo;
  {  Retrieves the file info from the current file.
     Called from the class myFileVersionInfo.
  }
  var
    FileVerInfo: TFileVersionInfo;
  begin
    FileVerInfo:=TFileVersionInfo.Create(nil);

    try
      FileVerInfo.ReadFileInfo;

      fileComments := FileVerInfo.VersionStrings.Values['Comments'];
      fileCompanyName := FileVerInfo.VersionStrings.Values['CompanyName'];
      fileFileDescription := FileVerInfo.VersionStrings.Values['FileDescription'];
      fileFileVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
      fileInternalName := FileVerInfo.VersionStrings.Values['InternalName'];
      fileLegalCopyright := FileVerInfo.VersionStrings.Values['LegalCopyright'];
      fileOriginalFileName := FileVerInfo.VersionStrings.Values['OriginalFilename'];
      fileProductName := FileVerInfo.VersionStrings.Values['ProductName'];
      fileProductVersion := FileVerInfo.VersionStrings.Values['ProductVersion'];
    finally
          FileVerInfo.Free;
    end;

  end;
   //
  //........................................ Helper functions ....................
  //
  Function Options.readChild(PassNode: TDOMNode;  name: string): string;
  {  Read a child node and return its [string] value.    }
  var
      childNode: TDOMNode;
  begin
      childNode := PassNode.FindNode(name);

      if assigned(childNode) then
        result := childNode.TextContent
      else
        result := 'ERROR';
  end;

  Function Options.readChildAttribute(PassNode: TDOMNode;  name: string; attribute: string): string;
  {  Read a child node and return its named [string] attribute.    }
  var
      childNode: TDOMNode;
  begin
    childNode := PassNode.FindNode(name);

    if assigned(childNode) then
      result := TDOMElement(childNode).GetAttribute(attribute)
    else
      result := 'ERROR';
  end;

  function Options.writeStrChild(Doc: TXMLDocument; name: string; value: string): TDOMNode;
  {  Write a [string] value to a child node.    }
  var
    ItemNode, TextNode: TDOMNode;
  begin
    ItemNode := Doc.CreateElement(name);
    TextNode := Doc.CreateTextNode(WideString(value));
    ItemNode.AppendChild(TextNode);
    result := ItemNode;
  end;

  function Options.writeColChild(Doc: TXMLDocument; name: string; value: TColor): TDOMNode;
  {  Write a [string] value to a child node.    }
  var
    ItemNode, TextNode: TDOMNode;
  begin
    ItemNode := Doc.CreateElement(name);
    TextNode := Doc.CreateTextNode(ColorToString(value));
    ItemNode.AppendChild(TextNode);
    result := ItemNode;
  end;

  function Options.writeFontChild(Doc: TXMLDocument; name: string; value: TFont): TDOMNode;
  {  Write a [font] value to a child node.    }
  var
    ItemNode, TextNode: TDOMNode;
  begin
    ItemNode := Doc.CreateElement(name);
    TextNode := Doc.CreateTextNode(FontToString(value));
    ItemNode.AppendChild(TextNode);
    result := ItemNode;
  end;

  function Options.writeDateChild(Doc: TXMLDocument; name: string; value: TDateTime): TDOMNode;
  {  Write a [date] value to a child node.    }
  var
    ItemNode: TDOMNode;
    TextNode: TDOMNode;
  begin
    ItemNode := Doc.CreateElement(name);
    TextNode := Doc.CreateTextNode(DateTimeToStr(value));
    ItemNode.AppendChild(TextNode);
    result := ItemNode;
  end;


  function Options.writeBolChild(Doc: TXMLDocument; name: string; value: boolean): TDOMNode;
  {  Write a [boolean] value to a child node.    }
  var
    ItemNode, TextNode: TDOMNode;
  begin
    ItemNode := Doc.CreateElement(name);
    TextNode := Doc.CreateTextNode(BoolToStr(value));
    ItemNode.AppendChild(TextNode);
    result := ItemNode;
  end;

  function Options.writeFloatChild(Doc: TXMLDocument; name: string; value: Double): TDOMNode;
  {  Write a [boolean] value to a child node.    }
  var
    ItemNode, TextNode: TDOMNode;
  begin
    ItemNode := Doc.CreateElement(name);
    TextNode := Doc.CreateTextNode(FloatToStr(value));
    ItemNode.AppendChild(TextNode);
    result := ItemNode;
  end;

  function Options.writeIntChild(Doc: TXMLDocument; name: string; value: integer): TDOMNode;
  {  Write a [integer] value to a child node.    }
  var
    ItemNode, TextNode: TDOMNode;
  begin
    ItemNode := Doc.CreateElement(name);
    TextNode := Doc.CreateTextNode(IntToStr(value));
    ItemNode.AppendChild(TextNode);
    result := ItemNode;
  end;

  function Options.writeIntChildAttribute(Doc: TXMLDocument; name: string; value1: integer; value2: integer): TDOMNode;
  {  Write a [integer] attribute to a child node.

     It seems you have to write both attributes at once, i can read them singularly.
     So, this routine is hard coded for form position until i can fix.
  }
  var
    ItemNode, TextNode: TDOMNode;
  begin
    ItemNode := Doc.CreateElement(name);
    TDOMElement(ItemNode).SetAttribute('Top', IntToStr(value1));
    TDOMElement(ItemNode).SetAttribute('Left', IntToStr(value2));
    TextNode := Doc.CreateTextNode('');
    ItemNode.AppendChild(TextNode);
    result := ItemNode;
  end;

end.


