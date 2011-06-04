unit VersInfo;
{
  Copyright (c) 1997 WinWright Consulting
  Written by Wayne Niddery

  You may use this code freely in any project, commercial included, as long
  as the this entire comment section, including copyright and credit, remains 
  intact. You may redistribute this code to others, and/or a compiled version
  thereof, as freeware only.
}
{
  Simple interface to version information
    - drop component on form.
    At Runtime:
      - Set the FileName property to the complete path of the target file
        or pass the name to the OpenFile procedure.
      - If interested in other than US English version info, then set
        the LanguageID property to the correct Language/CharSet value.
        A predefined constant - USEnglish - is the default value and can
        be used to return to the default. Other language constants may be
        added in future.
      - The standard (Windows defined) keys can be retrieved via the
        correspondingly named properties.
      - Non-standard keys can be retrieved via the GetKey function.
      - The standard keys are defined in the VersionInfoKeys constant array,
        these can also be passed to GetKey.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs;

type
  TVersionInfo = class(TComponent)
  private
    FFileName: string;
    FLanguageID: DWord;
    FInfo: pointer;
    FLang: PInteger;
    FLangNum: integer;
    FInfoSize: longint;
    FCtlCompanyName: TControl;
    FCtlFileDescription: TControl;
    FCtlFileVersion: TControl;
    FCtlInternalName: TControl;
    FCtlLegalCopyRight: TControl;
    FCtlOriginalFileName: TControl;
    FCtlProductName: TControl;
    FCtlProductVersion: TControl;
    procedure SetFileName(Value: string);
    procedure SetVerProp(index: integer; value: TControl);
    function GetVerProp(index: integer): TControl;
    function GetIndexKey(index: integer): string;
    procedure Refresh;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenFile(FName: string);
    procedure Close;
    function GetKey(const KName: string): string;
    property CompanyName: string index 1 read GetIndexKey;
    property FileDescription: string index 2 read GetIndexKey;
    property FileVersion: string index 3 read GetIndexKey;
    property InternalName: string index 4 read GetIndexKey;
    property LegalCopyRight: string index 5 read GetIndexKey;
    property OriginalFileName: string index 6 read GetIndexKey;
    property ProductName: string index 7 read GetIndexKey;
    property ProductVersion: string index 8 read GetIndexKey;
    property LanguageID: DWord read FLanguageID write FLanguageID;
    property FileName: string read FFileName write SetFileName;
  published
    property CtlCompanyName: TControl index 1 read GetVerProp write SetVerProp;
    property CtlFileDescription: TControl  index 2 read GetVerProp write SetVerProp;
    property CtlFileVersion: TControl  index 3 read GetVerProp write SetVerProp;
    property CtlInternalName: TControl  index 4 read GetVerProp write SetVerProp;
    property CtlLegalCopyRight: TControl  index 5 read GetVerProp write SetVerProp;
    property CtlOriginalFileName: TControl  index 6 read GetVerProp write SetVerProp;
    property CtlProductName: TControl  index 7 read GetVerProp write SetVerProp;
    property CtlProductVersion: TControl  index 8 read GetVerProp write SetVerProp;
  end;

const
  VersionInfoKeys: array [1..8] of string = (
    'CompanyName', 'FileDescription', 'FileVersion', 'InternalName',
    'LegalCopyRight', 'OriginalFileName', 'ProductName', 'ProductVersion'
    );

  USEnglish = $040904E4;

procedure Register;

implementation

uses TypInfo;

procedure Register;
begin
  RegisterComponents('WinWright', [TVersionInfo]);
end;

constructor TVersionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLanguageID := USEnglish;
//  SetFileName(EmptyStr);
  FFileName := EmptyStr;
  OpenFile(FFileName);
end;

destructor TVersionInfo.Destroy;
begin
  if FInfoSize > 0 then
    FreeMem(FInfo, FInfoSize);
  inherited Destroy;
end;

procedure TVersionInfo.SetFileName(Value: string);
begin
  FFileName := Value;
  if Value = EmptyStr then // default to self
    FFileName := Application.ExeName;
  if csDesigning in ComponentState then begin
    Refresh
  end else
    OpenFile(Value);
end;

procedure TVersionInfo.OpenFile(FName: string);
var
  vlen: DWord;
begin
  if FInfoSize > 0 then
    FreeMem(FInfo, FInfoSize);
  if Length(FName) <= 0 then FName := Application.ExeName;
  FInfoSize := GetFileVersionInfoSize(pchar(fname), vlen);
  if FInfoSize > 0 then begin
    GetMem(FInfo, FInfoSize);
    if not GetFileVersionInfo(pchar(fname), vlen, FInfoSize, FInfo) then
      raise Exception.Create('Cannot retrieve Version Information for ' + fname);
    // get languages
    VerQueryValue(FInfo, '\VarFileInfo\Translation', pointer(FLang), vlen);
    FLangNum := vlen div 4;
    Refresh;
  end;
end;

procedure TVersionInfo.Close;
begin
  if FInfoSize > 0 then
    FreeMem(FInfo, FInfoSize);
  FInfoSize := 0;
  FFileName := EmptyStr;
end;

const
  vqvFmt = '\StringFileInfo\%4.4x%4.4x\%s';

function TVersionInfo.GetKey(const KName: string): string;
var
  vptr: pchar;
  vlen: DWord;
begin
  Result := EmptyStr;
  if FInfoSize <= 0 then exit;
  if VerQueryValue(FInfo, pchar(Format(vqvFmt, [LoWord(FLang^), HiWord(FLang^), KName])), pointer(vptr), vlen) then
    Result := vptr;
end;

function TVersionInfo.GetIndexKey(index: integer): string;
begin
  Result := GetKey(VersionInfoKeys[index]);
end;

function TVersionInfo.GetVerProp(index: integer): TControl;
begin
  case index of
    1: Result := FCtlCompanyName;
    2: Result := FCtlFileDescription;
    3: Result := FCtlFileVersion;
    4: Result := FCtlInternalName;
    5: Result := FCtlLegalCopyRight;
    6: Result := FCtlOriginalFileName;
    7: Result := FCtlProductName;
    8: Result := FCtlProductVersion;
    else Result := nil;
  end;
end;

procedure TVersionInfo.SetVerProp(index: integer; value: TControl);
begin
  case index of
    1: FCtlCompanyName := Value;
    2: FCtlFileDescription := Value;
    3: FCtlFileVersion := Value;
    4: FCtlInternalName := Value;
    5: FCtlLegalCopyRight := Value;
    6: FCtlOriginalFileName := Value;
    7: FCtlProductName := Value;
    8: FCtlProductVersion := Value;
  end;
  Refresh;
end;

procedure TVersionInfo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FCtlCompanyName then FCtlCompanyName := nil
    else if AComponent = FCtlFileDescription then FCtlFileDescription := nil
    else if AComponent = FCtlFileVersion then FCtlFileVersion := nil
    else if AComponent = FCtlInternalName then FCtlInternalName := nil
    else if AComponent = FCtlLegalCopyRight then FCtlLegalCopyRight := nil
    else if AComponent = FCtlOriginalFileName then FCtlOriginalFileName := nil
    else if AComponent = FCtlProductName then FCtlProductName := nil
    else if AComponent = FCtlProductVersion then FCtlProductVersion := nil;
  end;
end;

procedure TVersionInfo.Refresh;
var PropInfo: PPropInfo;

  procedure AssignText(Actl: TComponent; txt: string);
  begin
    if Assigned(ACtl) then begin
      PropInfo := GetPropInfo(ACtl.ClassInfo, 'Caption');
      if PropInfo <> nil then
        SetStrProp(ACtl, PropInfo, txt)
      else begin
        PropInfo := GetPropInfo(ACtl.ClassInfo, 'Text');
        if PropInfo <> nil then
          SetStrProp(ACtl, PropInfo, txt)
      end;
    end;
  end;

begin
  if csDesigning in ComponentState then begin
    AssignText(FCtlCompanyName, VersionInfoKeys[1]);
    AssignText(FCtlFileDescription, VersionInfoKeys[2]);
    AssignText(FCtlFileVersion, VersionInfoKeys[3]);
    AssignText(FCtlInternalName, VersionInfoKeys[4]);
    AssignText(FCtlLegalCopyRight, VersionInfoKeys[5]);
    AssignText(FCtlOriginalFileName, VersionInfoKeys[6]);
    AssignText(FCtlProductName, VersionInfoKeys[7]);
    AssignText(FCtlProductVersion, VersionInfoKeys[8]);
  end else begin
    AssignText(FCtlCompanyName, CompanyName);
    AssignText(FCtlFileDescription, FileDescription);
    AssignText(FCtlFileVersion, FileVersion);
    AssignText(FCtlInternalName, InternalName);
    AssignText(FCtlLegalCopyRight, LegalCopyRight);
    AssignText(FCtlOriginalFileName, OriginalFileName);
    AssignText(FCtlProductName, ProductName);
    AssignText(FCtlProductVersion, ProductVersion);
  end;
end;

end.
