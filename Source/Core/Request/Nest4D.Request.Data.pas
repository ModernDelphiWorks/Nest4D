unit Nest4D.Request.Data;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.Generics.Collections;

type
  TKeyValue = TDictionary<String, TValue>;
  TFiles = TDictionary<String, TStream>;
  TArrayPair = array of TPair<String, String>;

  TRequestData = class
  private
    FKeyValue: TKeyValue;
    FFiles: TFiles;
    FContent: TStrings;
    FRequired: Boolean;
    function _GetItem(const AKey: String): TValue;
    function _GetDictionary: TKeyValue;
    function _GetCount: Int16;
    function _GetContent: TStrings;
    function _AsString(const AKey: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const AContent: TStrings);
    procedure AddOrSetValue(const AKey: String; const AValue: TValue);
    function Required(const AValue: Boolean): TRequestData;
    function ContainsKey(const AKey: String): Boolean;
    function ContainsValue(const AValue: String): Boolean;
    function ToArray: TArrayPair;
    function TryGetValue(const AKey: String; var AValue: String): Boolean;
    function AddStream(const AKey: String; const AContent: TStream): TRequestData;
    function Value<T>(const AKey: String): T;
    property Content: TStrings read _GetContent;
    property Count: Int16 read _GetCount;
    property Items[const AKey: String]: TValue read _GetItem; default;
    property Dictionary: TKeyValue read _GetDictionary;
  end;

implementation

function TRequestData.ContainsKey(const AKey: String): Boolean;
begin
  Result := FKeyValue.ContainsKey(AKey);
end;

function TRequestData.ContainsValue(const AValue: String): Boolean;
begin
  Result := FKeyValue.ContainsValue(AValue);
end;

constructor TRequestData.Create;
begin
  FKeyValue := TKeyValue.Create;
  FContent := TStringList.Create;
  FRequired := False;
end;

destructor TRequestData.Destroy;
begin
  FKeyValue.Free;
  FContent.Free;
  if Assigned(FFiles) then
    FFiles.Free;
  inherited;
end;

procedure TRequestData.AddOrSetValue(const AKey: String; const AValue: TValue);
begin
  FKeyValue.AddOrSetValue(Akey, AValue);
end;

function TRequestData.AddStream(const AKey: String; const AContent: TStream): TRequestData;
begin
  Result := Self;
  if not Assigned(FFiles) then
    FFiles := TFiles.Create;
  FFiles.AddOrSetValue(AKey, AContent);
end;

function TRequestData._AsString(const AKey: String): String;
var
  LKey: String;
begin
  Result := EmptyStr;
  for LKey in FKeyValue.Keys do
  begin
    if AnsiCompareText(LKey, AKey) = 0 then
      Exit(FKeyValue.Items[LKey].AsString);
  end;
end;

function TRequestData._GetContent: TStrings;
var
  LKey: String;
begin
  for LKey in FKeyValue.Keys do
    FContent.Add(Format('%s=%s', [LKey, FKeyValue[LKey].AsString]));
  Result := FContent;
end;

procedure TRequestData.Assign(const AContent: TStrings);
var
  LFor: Int16;
  LKey: String;
  LValue: String;
  LSeparatorPos: Int16;
begin
  FContent.Assign(AContent);
  FKeyValue.Clear;
  for LFor := 0 to FContent.Count - 1 do
  begin
    LSeparatorPos := Pos('=', FContent[LFor]);
    if LSeparatorPos = 0 then
      Continue;
    LKey := Trim(Copy(FContent[LFor], 1, LSeparatorPos - 1));
    LValue := Trim(Copy(FContent[LFor], LSeparatorPos + 1, Length(FContent[LFor])));
    FKeyValue.AddOrSetValue(LKey, LValue);
  end;
end;

function TRequestData._GetCount: Int16;
begin
  Result := FKeyValue.Count;
end;

function TRequestData._GetItem(const AKey: String): TValue;
var
  LKey: String;
begin
  for LKey in FKeyValue.Keys do
  begin
    if AnsiCompareText(LKey, AKey) = 0 then
      Exit(FKeyValue[LKey]);
  end;
  Result := EmptyStr;
end;

function TRequestData.Required(const AValue: Boolean): TRequestData;
begin
  Result := Self;
  FRequired := AValue;
end;

function TRequestData._GetDictionary: TKeyValue;
begin
  Result := FKeyValue;
end;

function TRequestData.ToArray: TArrayPair;
var
  LArrayPair: TArrayPair;
  LPair: TPair<String, TValue>;
  LIndex: Int16;
begin
  SetLength(LArrayPair, FKeyValue.Count);
  LIndex := 0;
  for LPair in FKeyValue do
  begin
    LArrayPair[LIndex].Key := LPair.Key;
    LArrayPair[LIndex].Value := LPair.Value.AsString;
    Inc(LIndex);
  end;
  Result := LArrayPair;
end;

function TRequestData.TryGetValue(const AKey: String; var AValue: String): Boolean;
begin
  Result := ContainsKey(AKey);
  if Result then
    AValue := _AsString(AKey);
end;

function TRequestData.Value<T>(const AKey: String): T;
begin
  Result := Default(T);
  if not FKeyValue.ContainsKey(AKey) then
    Exit;
  Result := FKeyValue.Items[Akey].AsType<T>;
end;

end.
