# Exemplo: Sistema de Plugins (Transform Pipes)

Este exemplo demonstra como usar e criar plugins Transform Pipes no Nest4D para serialização/deserialização de dados em diferentes formatos.

## 📋 Plugins Disponíveis

### 1. Plugin JsonBr (Incluído)

O Nest4D inclui o plugin JsonBr para serialização JSON otimizada:

```pascal
// Exemplo de uso do JsonBr
uses
  nest4d.pipes.transforms.jsonbr;

var
  JsonBrPipe: ITransformPipe;
  Person: TPerson;
  JsonResult: TTransformResult;
  PersonResult: TTransformResult;
begin
  JsonBrPipe := TJsonBrTransformPipe.Create;
  
  // Serializar objeto para JSON
  Person := TPerson.Create;
  Person.Name := 'João Silva';
  Person.Age := 30;
  Person.Email := 'joao@email.com';
  
  JsonResult := JsonBrPipe.Transform(Person);
  if JsonResult.Success then
    WriteLn('JSON: ' + JsonResult.Data)
  else
    WriteLn('Erro: ' + JsonResult.ErrorMessage);
  
  // Deserializar JSON para objeto
  PersonResult := JsonBrPipe.ReverseTransform(JsonResult.Data, TPerson);
  if PersonResult.Success then
  begin
    Person := PersonResult.Data as TPerson;
    WriteLn('Nome: ' + Person.Name);
  end;
end;
```

## 🔧 Criando Plugins Customizados

### 1. Plugin XML Transform

```pascal
// XmlTransformPipe.pas
unit XmlTransformPipe;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  Xml.XMLDoc,
  Xml.XMLIntf,
  nest4d.pipes.transforms;

type
  TXmlTransformPipe = class(TTransformPipe)
  private
    function ObjectToXml(AObject: TObject): string;
    function XmlToObject(const XmlData: string; AClass: TClass): TObject;
    function GetXmlNodeValue(Node: IXMLNode; const NodeName: string): string;
    procedure SetObjectProperty(AObject: TObject; const PropName, Value: string);
  public
    function Transform(const Input: TValue): TTransformResult; override;
    function ReverseTransform(const Input: string; TargetType: TClass): TTransformResult; override;
    function GetSupportedFormats: TArray<string>; override;
    function GetPipeName: string; override;
  end;

implementation

function TXmlTransformPipe.GetPipeName: string;
begin
  Result := 'XML Transform Pipe';
end;

function TXmlTransformPipe.GetSupportedFormats: TArray<string>;
begin
  Result := ['xml', 'application/xml', 'text/xml'];
end;

function TXmlTransformPipe.Transform(const Input: TValue): TTransformResult;
var
  XmlData: string;
begin
  try
    if Input.IsObject then
    begin
      XmlData := ObjectToXml(Input.AsObject);
      Result := TTransformResult.Success(XmlData);
    end
    else
    begin
      Result := TTransformResult.Failure('Input must be an object');
    end;
  except
    on E: Exception do
      Result := TTransformResult.Failure('XML serialization failed: ' + E.Message);
  end;
end;

function TXmlTransformPipe.ReverseTransform(const Input: string; TargetType: TClass): TTransformResult;
var
  Obj: TObject;
begin
  try
    Obj := XmlToObject(Input, TargetType);
    if Assigned(Obj) then
      Result := TTransformResult.Success(TValue.From<TObject>(Obj))
    else
      Result := TTransformResult.Failure('Failed to create object from XML');
  except
    on E: Exception do
      Result := TTransformResult.Failure('XML deserialization failed: ' + E.Message);
  end;
end;

function TXmlTransformPipe.ObjectToXml(AObject: TObject): string;
var
  XmlDoc: IXMLDocument;
  RootNode, PropNode: IXMLNode;
  Context: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Value: TValue;
begin
  XmlDoc := TXMLDocument.Create(nil);
  XmlDoc.Active := True;
  
  // Criar nó raiz com o nome da classe
  RootNode := XmlDoc.AddChild(AObject.ClassName);
  
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AObject.ClassType);
    
    // Iterar pelas propriedades do objeto
    for Prop in RttiType.GetProperties do
    begin
      if Prop.IsReadable then
      begin
        Value := Prop.GetValue(AObject);
        PropNode := RootNode.AddChild(Prop.Name);
        
        case Value.Kind of
          tkInteger, tkInt64:
            PropNode.Text := IntToStr(Value.AsInteger);
          tkFloat:
            PropNode.Text := FloatToStr(Value.AsExtended);
          tkString, tkLString, tkWString, tkUString:
            PropNode.Text := Value.AsString;
          tkEnumeration:
            if Value.TypeInfo = TypeInfo(Boolean) then
              PropNode.Text := BoolToStr(Value.AsBoolean, True)
            else
              PropNode.Text := Value.ToString;
        else
          PropNode.Text := Value.ToString;
        end;
      end;
    end;
    
    Result := XmlDoc.XML.Text;
  finally
    Context.Free;
  end;
end;

function TXmlTransformPipe.XmlToObject(const XmlData: string; AClass: TClass): TObject;
var
  XmlDoc: IXMLDocument;
  RootNode, PropNode: IXMLNode;
  Context: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  I: Integer;
begin
  Result := nil;
  
  XmlDoc := TXMLDocument.Create(nil);
  try
    XmlDoc.LoadFromXML(XmlData);
    XmlDoc.Active := True;
    
    RootNode := XmlDoc.DocumentElement;
    if not Assigned(RootNode) then
      Exit;
    
    // Criar instância do objeto
    Result := AClass.Create;
    
    Context := TRttiContext.Create;
    try
      RttiType := Context.GetType(AClass);
      
      // Mapear propriedades XML para objeto
      for I := 0 to RootNode.ChildNodes.Count - 1 do
      begin
        PropNode := RootNode.ChildNodes[I];
        Prop := RttiType.GetProperty(PropNode.NodeName);
        
        if Assigned(Prop) and Prop.IsWritable then
        begin
          SetObjectProperty(Result, Prop.Name, PropNode.Text);
        end;
      end;
    finally
      Context.Free;
    end;
    
  except
    on E: Exception do
    begin
      if Assigned(Result) then
      begin
        Result.Free;
        Result := nil;
      end;
      raise;
    end;
  end;
end;

function TXmlTransformPipe.GetXmlNodeValue(Node: IXMLNode; const NodeName: string): string;
var
  ChildNode: IXMLNode;
begin
  Result := '';
  ChildNode := Node.ChildNodes.FindNode(NodeName);
  if Assigned(ChildNode) then
    Result := ChildNode.Text;
end;

procedure TXmlTransformPipe.SetObjectProperty(AObject: TObject; const PropName, Value: string);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  PropValue: TValue;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AObject.ClassType);
    Prop := RttiType.GetProperty(PropName);
    
    if Assigned(Prop) and Prop.IsWritable then
    begin
      case Prop.PropertyType.TypeKind of
        tkInteger, tkInt64:
          PropValue := StrToIntDef(Value, 0);
        tkFloat:
          PropValue := StrToFloatDef(Value, 0.0);
        tkString, tkLString, tkWString, tkUString:
          PropValue := Value;
        tkEnumeration:
          if Prop.PropertyType.Handle = TypeInfo(Boolean) then
            PropValue := StrToBoolDef(Value, False)
          else
            PropValue := TValue.FromOrdinal(Prop.PropertyType.Handle, StrToIntDef(Value, 0));
      else
        PropValue := Value;
      end;
      
      Prop.SetValue(AObject, PropValue);
    end;
  finally
    Context.Free;
  end;
end;

end.
```

### 2. Plugin YAML Transform

```pascal
// YamlTransformPipe.pas
unit YamlTransformPipe;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  nest4d.pipes.transforms;

type
  TYamlTransformPipe = class(TTransformPipe)
  private
    function ObjectToYaml(AObject: TObject; IndentLevel: Integer = 0): string;
    function YamlToObject(const YamlData: string; AClass: TClass): TObject;
    function ParseYamlValue(const Value: string): TValue;
    function GetIndent(Level: Integer): string;
    function ExtractYamlProperties(const YamlData: string): TDictionary<string, string>;
  public
    function Transform(const Input: TValue): TTransformResult; override;
    function ReverseTransform(const Input: string; TargetType: TClass): TTransformResult; override;
    function GetSupportedFormats: TArray<string>; override;
    function GetPipeName: string; override;
  end;

implementation

function TYamlTransformPipe.GetPipeName: string;
begin
  Result := 'YAML Transform Pipe';
end;

function TYamlTransformPipe.GetSupportedFormats: TArray<string>;
begin
  Result := ['yaml', 'yml', 'application/yaml', 'text/yaml'];
end;

function TYamlTransformPipe.Transform(const Input: TValue): TTransformResult;
var
  YamlData: string;
begin
  try
    if Input.IsObject then
    begin
      YamlData := ObjectToYaml(Input.AsObject);
      Result := TTransformResult.Success(YamlData);
    end
    else
    begin
      Result := TTransformResult.Failure('Input must be an object');
    end;
  except
    on E: Exception do
      Result := TTransformResult.Failure('YAML serialization failed: ' + E.Message);
  end;
end;

function TYamlTransformPipe.ReverseTransform(const Input: string; TargetType: TClass): TTransformResult;
var
  Obj: TObject;
begin
  try
    Obj := YamlToObject(Input, TargetType);
    if Assigned(Obj) then
      Result := TTransformResult.Success(TValue.From<TObject>(Obj))
    else
      Result := TTransformResult.Failure('Failed to create object from YAML');
  except
    on E: Exception do
      Result := TTransformResult.Failure('YAML deserialization failed: ' + E.Message);
  end;
end;

function TYamlTransformPipe.ObjectToYaml(AObject: TObject; IndentLevel: Integer): string;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Value: TValue;
  Indent: string;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Indent := GetIndent(IndentLevel);
    Context := TRttiContext.Create;
    
    try
      RttiType := Context.GetType(AObject.ClassType);
      
      for Prop in RttiType.GetProperties do
      begin
        if Prop.IsReadable then
        begin
          Value := Prop.GetValue(AObject);
          
          case Value.Kind of
            tkInteger, tkInt64:
              Lines.Add(Indent + Prop.Name + ': ' + IntToStr(Value.AsInteger));
            tkFloat:
              Lines.Add(Indent + Prop.Name + ': ' + FloatToStr(Value.AsExtended));
            tkString, tkLString, tkWString, tkUString:
              Lines.Add(Indent + Prop.Name + ': "' + Value.AsString + '"');
            tkEnumeration:
              if Value.TypeInfo = TypeInfo(Boolean) then
                Lines.Add(Indent + Prop.Name + ': ' + BoolToStr(Value.AsBoolean, True).ToLower)
              else
                Lines.Add(Indent + Prop.Name + ': ' + Value.ToString);
          else
            Lines.Add(Indent + Prop.Name + ': "' + Value.ToString + '"');
          end;
        end;
      end;
      
      Result := Lines.Text;
    finally
      Context.Free;
    end;
  finally
    Lines.Free;
  end;
end;

function TYamlTransformPipe.YamlToObject(const YamlData: string; AClass: TClass): TObject;
var
  Properties: TDictionary<string, string>;
  Context: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  PropValue: string;
  Value: TValue;
begin
  Result := nil;
  Properties := ExtractYamlProperties(YamlData);
  
  try
    Result := AClass.Create;
    Context := TRttiContext.Create;
    
    try
      RttiType := Context.GetType(AClass);
      
      for Prop in RttiType.GetProperties do
      begin
        if Prop.IsWritable and Properties.TryGetValue(Prop.Name, PropValue) then
        begin
          Value := ParseYamlValue(PropValue);
          
          case Prop.PropertyType.TypeKind of
            tkInteger, tkInt64:
              Prop.SetValue(Result, StrToIntDef(PropValue, 0));
            tkFloat:
              Prop.SetValue(Result, StrToFloatDef(PropValue, 0.0));
            tkString, tkLString, tkWString, tkUString:
              Prop.SetValue(Result, PropValue.DeQuotedString('"'));
            tkEnumeration:
              if Prop.PropertyType.Handle = TypeInfo(Boolean) then
                Prop.SetValue(Result, SameText(PropValue, 'true'))
              else
                Prop.SetValue(Result, TValue.FromOrdinal(Prop.PropertyType.Handle, StrToIntDef(PropValue, 0)));
          else
            Prop.SetValue(Result, PropValue);
          end;
        end;
      end;
    finally
      Context.Free;
    end;
  finally
    Properties.Free;
  end;
end;

function TYamlTransformPipe.ParseYamlValue(const Value: string): TValue;
var
  TrimmedValue: string;
begin
  TrimmedValue := Value.Trim;
  
  // Tentar converter para diferentes tipos
  if SameText(TrimmedValue, 'true') or SameText(TrimmedValue, 'false') then
    Result := SameText(TrimmedValue, 'true')
  else if TrimmedValue.StartsWith('"') and TrimmedValue.EndsWith('"') then
    Result := TrimmedValue.DeQuotedString('"')
  else if TrimmedValue.Contains('.') then
    Result := StrToFloatDef(TrimmedValue, 0.0)
  else
    Result := StrToIntDef(TrimmedValue, 0);
end;

function TYamlTransformPipe.GetIndent(Level: Integer): string;
begin
  Result := StringOfChar(' ', Level * 2);
end;

function TYamlTransformPipe.ExtractYamlProperties(const YamlData: string): TDictionary<string, string>;
var
  Lines: TStringList;
  Line, Key, Value: string;
  ColonPos: Integer;
  I: Integer;
begin
  Result := TDictionary<string, string>.Create;
  Lines := TStringList.Create;
  
  try
    Lines.Text := YamlData;
    
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I].Trim;
      if (Line <> '') and not Line.StartsWith('#') then
      begin
        ColonPos := Line.IndexOf(':');
        if ColonPos > 0 then
        begin
          Key := Line.Substring(0, ColonPos).Trim;
          Value := Line.Substring(ColonPos + 1).Trim;
          Result.AddOrSetValue(Key, Value);
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

end.
```

### 3. Plugin CSV Transform

```pascal
// CsvTransformPipe.pas
unit CsvTransformPipe;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  nest4d.pipes.transforms;

type
  TCsvTransformPipe = class(TTransformPipe)
  private
    FSeparator: Char;
    FQuoteChar: Char;
    FIncludeHeaders: Boolean;
    
    function ObjectToCsv(AObject: TObject): string;
    function ObjectListToCsv(AObjectList: TObjectList<TObject>): string;
    function CsvToObjectList(const CsvData: string; AClass: TClass): TObjectList<TObject>;
    function ParseCsvLine(const Line: string): TArray<string>;
    function EscapeCsvValue(const Value: string): string;
  public
    constructor Create(Separator: Char = ','; QuoteChar: Char = '"'; IncludeHeaders: Boolean = True);
    
    function Transform(const Input: TValue): TTransformResult; override;
    function ReverseTransform(const Input: string; TargetType: TClass): TTransformResult; override;
    function GetSupportedFormats: TArray<string>; override;
    function GetPipeName: string; override;
    
    property Separator: Char read FSeparator write FSeparator;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
    property IncludeHeaders: Boolean read FIncludeHeaders write FIncludeHeaders;
  end;

implementation

constructor TCsvTransformPipe.Create(Separator: Char; QuoteChar: Char; IncludeHeaders: Boolean);
begin
  inherited Create;
  FSeparator := Separator;
  FQuoteChar := QuoteChar;
  FIncludeHeaders := IncludeHeaders;
end;

function TCsvTransformPipe.GetPipeName: string;
begin
  Result := 'CSV Transform Pipe';
end;

function TCsvTransformPipe.GetSupportedFormats: TArray<string>;
begin
  Result := ['csv', 'text/csv', 'application/csv'];
end;

function TCsvTransformPipe.Transform(const Input: TValue): TTransformResult;
var
  CsvData: string;
begin
  try
    if Input.IsObject then
    begin
      if Input.AsObject is TObjectList<TObject> then
        CsvData := ObjectListToCsv(Input.AsObject as TObjectList<TObject>)
      else
        CsvData := ObjectToCsv(Input.AsObject);
        
      Result := TTransformResult.Success(CsvData);
    end
    else
    begin
      Result := TTransformResult.Failure('Input must be an object or object list');
    end;
  except
    on E: Exception do
      Result := TTransformResult.Failure('CSV serialization failed: ' + E.Message);
  end;
end;

function TCsvTransformPipe.ReverseTransform(const Input: string; TargetType: TClass): TTransformResult;
var
  ObjectList: TObjectList<TObject>;
begin
  try
    ObjectList := CsvToObjectList(Input, TargetType);
    if Assigned(ObjectList) then
      Result := TTransformResult.Success(TValue.From<TObjectList<TObject>>(ObjectList))
    else
      Result := TTransformResult.Failure('Failed to create object list from CSV');
  except
    on E: Exception do
      Result := TTransformResult.Failure('CSV deserialization failed: ' + E.Message);
  end;
end;

function TCsvTransformPipe.ObjectToCsv(AObject: TObject): string;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Properties: TArray<TRttiProperty>;
  Prop: TRttiProperty;
  Value: TValue;
  Headers, Values: TStringList;
  I: Integer;
begin
  Headers := TStringList.Create;
  Values := TStringList.Create;
  
  try
    Context := TRttiContext.Create;
    try
      RttiType := Context.GetType(AObject.ClassType);
      Properties := RttiType.GetProperties;
      
      // Coletar headers e valores
      for Prop in Properties do
      begin
        if Prop.IsReadable then
        begin
          Headers.Add(Prop.Name);
          Value := Prop.GetValue(AObject);
          Values.Add(EscapeCsvValue(Value.ToString));
        end;
      end;
      
      // Montar CSV
      if FIncludeHeaders then
        Result := Headers.DelimitedText + sLineBreak;
        
      Values.Delimiter := FSeparator;
      Values.QuoteChar := FQuoteChar;
      Result := Result + Values.DelimitedText;
      
    finally
      Context.Free;
    end;
  finally
    Headers.Free;
    Values.Free;
  end;
end;

function TCsvTransformPipe.ObjectListToCsv(AObjectList: TObjectList<TObject>): string;
var
  Lines: TStringList;
  Obj: TObject;
  FirstObject: Boolean;
begin
  Lines := TStringList.Create;
  try
    FirstObject := True;
    
    for Obj in AObjectList do
    begin
      if FirstObject then
      begin
        Lines.Add(ObjectToCsv(Obj));
        FirstObject := False;
      end
      else
      begin
        // Para objetos subsequentes, não incluir headers
        FIncludeHeaders := False;
        Lines.Add(ObjectToCsv(Obj));
        FIncludeHeaders := True;
      end;
    end;
    
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TCsvTransformPipe.CsvToObjectList(const CsvData: string; AClass: TClass): TObjectList<TObject>;
var
  Lines: TStringList;
  Headers: TArray<string>;
  Values: TArray<string>;
  Obj: TObject;
  Context: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  I, J: Integer;
  StartLine: Integer;
begin
  Result := TObjectList<TObject>.Create(True);
  Lines := TStringList.Create;
  
  try
    Lines.Text := CsvData;
    
    if Lines.Count = 0 then
      Exit;
    
    Context := TRttiContext.Create;
    try
      RttiType := Context.GetType(AClass);
      
      // Determinar se há headers
      StartLine := 0;
      if FIncludeHeaders and (Lines.Count > 0) then
      begin
        Headers := ParseCsvLine(Lines[0]);
        StartLine := 1;
      end
      else
      begin
        // Usar nomes das propriedades como headers
        SetLength(Headers, 0);
        for Prop in RttiType.GetProperties do
        begin
          if Prop.IsReadable then
          begin
            SetLength(Headers, Length(Headers) + 1);
            Headers[High(Headers)] := Prop.Name;
          end;
        end;
      end;
      
      // Processar linhas de dados
      for I := StartLine to Lines.Count - 1 do
      begin
        if Lines[I].Trim <> '' then
        begin
          Values := ParseCsvLine(Lines[I]);
          Obj := AClass.Create;
          
          try
            // Mapear valores para propriedades
            for J := 0 to Min(High(Headers), High(Values)) do
            begin
              Prop := RttiType.GetProperty(Headers[J]);
              if Assigned(Prop) and Prop.IsWritable then
              begin
                case Prop.PropertyType.TypeKind of
                  tkInteger, tkInt64:
                    Prop.SetValue(Obj, StrToIntDef(Values[J], 0));
                  tkFloat:
                    Prop.SetValue(Obj, StrToFloatDef(Values[J], 0.0));
                  tkString, tkLString, tkWString, tkUString:
                    Prop.SetValue(Obj, Values[J]);
                  tkEnumeration:
                    if Prop.PropertyType.Handle = TypeInfo(Boolean) then
                      Prop.SetValue(Obj, StrToBoolDef(Values[J], False));
                end;
              end;
            end;
            
            Result.Add(Obj);
          except
            Obj.Free;
            raise;
          end;
        end;
      end;
    finally
      Context.Free;
    end;
  finally
    Lines.Free;
  end;
end;

function TCsvTransformPipe.ParseCsvLine(const Line: string): TArray<string>;
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    Values.Delimiter := FSeparator;
    Values.QuoteChar := FQuoteChar;
    Values.DelimitedText := Line;
    
    SetLength(Result, Values.Count);
    for var I := 0 to Values.Count - 1 do
      Result[I] := Values[I];
  finally
    Values.Free;
  end;
end;

function TCsvTransformPipe.EscapeCsvValue(const Value: string): string;
begin
  Result := Value;
  
  // Se contém separador, quebra de linha ou aspas, precisa ser quoted
  if (Result.Contains(FSeparator)) or (Result.Contains(sLineBreak)) or (Result.Contains(FQuoteChar)) then
  begin
    // Escapar aspas duplicando
    Result := Result.Replace(FQuoteChar, FQuoteChar + FQuoteChar);
    // Envolver em aspas
    Result := FQuoteChar + Result + FQuoteChar;
  end;
end;

end.
```

## 🔧 Usando os Plugins

### 1. Servidor com Múltiplos Plugins

```pascal
program PluginServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  nest4d.core.async,
  nest4d.horse.async,
  nest4d.pipes.transforms,
  nest4d.pipes.transforms.jsonbr,
  XmlTransformPipe,
  YamlTransformPipe,
  CsvTransformPipe;

type
  TPerson = class
  private
    FName: string;
    FAge: Integer;
    FEmail: string;
    FActive: Boolean;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
    property Active: Boolean read FActive write FActive;
  end;

var
  JsonBrPipe: ITransformPipe;
  XmlPipe: ITransformPipe;
  YamlPipe: ITransformPipe;
  CsvPipe: ITransformPipe;
  PipeRegistry: TDictionary<string, ITransformPipe>;

function GetPipeByFormat(const Format: string): ITransformPipe;
begin
  if PipeRegistry.TryGetValue(Format.ToLower, Result) then
    Exit;
    
  // Fallback para JSON
  Result := JsonBrPipe;
end;

function CreateSamplePerson: TPerson;
begin
  Result := TPerson.Create;
  Result.Name := 'João Silva';
  Result.Age := 30;
  Result.Email := 'joao@email.com';
  Result.Active := True;
end;

begin
  try
    WriteLn('=== Nest4D Plugin Transform Server ===');
    WriteLn('');
    
    // Inicializar plugins
    JsonBrPipe := TJsonBrTransformPipe.Create;
    XmlPipe := TXmlTransformPipe.Create;
    YamlPipe := TYamlTransformPipe.Create;
    CsvPipe := TCsvTransformPipe.Create;
    
    // Registrar plugins
    PipeRegistry := TDictionary<string, ITransformPipe>.Create;
    PipeRegistry.Add('json', JsonBrPipe);
    PipeRegistry.Add('application/json', JsonBrPipe);
    PipeRegistry.Add('xml', XmlPipe);
    PipeRegistry.Add('application/xml', XmlPipe);
    PipeRegistry.Add('yaml', YamlPipe);
    PipeRegistry.Add('yml', YamlPipe);
    PipeRegistry.Add('csv', CsvPipe);
    PipeRegistry.Add('text/csv', CsvPipe);
    
    TNest4D
      .Create
      
      // Endpoint para listar plugins disponíveis
      .Get('/api/plugins', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        var
          Plugins: TJSONArray;
          Plugin: TJSONObject;
          Pipe: ITransformPipe;
          Format: string;
        begin
          Plugins := TJSONArray.Create;
          try
            for Format in PipeRegistry.Keys do
            begin
              Pipe := PipeRegistry[Format];
              Plugin := TJSONObject.Create;
              Plugin.AddPair('format', Format);
              Plugin.AddPair('name', Pipe.GetPipeName);
              Plugins.AddElement(Plugin);
            end;
            
            Res.Send(Plugins.ToString);
          finally
            Plugins.Free;
          end;
        end)
      
      // Endpoint para transformar dados
      .Post('/api/transform/:format', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        var
          Format: string;
          Pipe: ITransformPipe;
          Person: TPerson;
          TransformResult: TTransformResult;
        begin
          Format := Req.Params['format'];
          Pipe := GetPipeByFormat(Format);
          
          Person := CreateSamplePerson;
          try
            TransformResult := Pipe.Transform(TValue.From<TObject>(Person));
            
            if TransformResult.Success then
            begin
              // Definir Content-Type apropriado
              case Format.ToLower of
                'xml': Res.AddHeader('Content-Type', 'application/xml');
                'yaml', 'yml': Res.AddHeader('Content-Type', 'application/yaml');
                'csv': Res.AddHeader('Content-Type', 'text/csv');
              else
                Res.AddHeader('Content-Type', 'application/json');
              end;
              
              Res.Send(TransformResult.Data);
            end
            else
            begin
              Res.Status(500).Send(Format('{"error": "%s"}', [TransformResult.ErrorMessage]));
            end;
          finally
            Person.Free;
          end;
        end)
      
      // Endpoint para converter entre formatos
      .Post('/api/convert/:from/:to', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        var
          FromFormat, ToFormat: string;
          FromPipe, ToPipe: ITransformPipe;
          InputData: string;
          ReverseResult, TransformResult: TTransformResult;
          TempObject: TObject;
        begin
          FromFormat := Req.Params['from'];
          ToFormat := Req.Params['to'];
          InputData := Req.Body;
          
          FromPipe := GetPipeByFormat(FromFormat);
          ToPipe := GetPipeByFormat(ToFormat);
          
          try
            // Deserializar do formato origem
            ReverseResult := FromPipe.ReverseTransform(InputData, TPerson);
            
            if ReverseResult.Success then
            begin
              TempObject := ReverseResult.Data.AsObject;
              try
                // Serializar para formato destino
                TransformResult := ToPipe.Transform(TValue.From<TObject>(TempObject));
                
                if TransformResult.Success then
                begin
                  // Definir Content-Type
                  case ToFormat.ToLower of
                    'xml': Res.AddHeader('Content-Type', 'application/xml');
                    'yaml', 'yml': Res.AddHeader('Content-Type', 'application/yaml');
                    'csv': Res.AddHeader('Content-Type', 'text/csv');
                  else
                    Res.AddHeader('Content-Type', 'application/json');
                  end;
                  
                  Res.Send(TransformResult.Data);
                end
                else
                begin
                  Res.Status(500).Send(Format('{"error": "Serialization failed: %s"}', 
                                             [TransformResult.ErrorMessage]));
                end;
              finally
                TempObject.Free;
              end;
            end
            else
            begin
              Res.Status(400).Send(Format('{"error": "Deserialization failed: %s"}', 
                                         [ReverseResult.ErrorMessage]));
            end;
          except
            on E: Exception do
              Res.Status(500).Send(Format('{"error": "Conversion failed: %s"}', [E.Message]));
          end;
        end)
      
      // Endpoint para validar formato
      .Post('/api/validate/:format', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        var
          Format: string;
          Pipe: ITransformPipe;
          InputData: string;
          ReverseResult: TTransformResult;
          ValidationResult: TJSONObject;
        begin
          Format := Req.Params['format'];
          InputData := Req.Body;
          Pipe := GetPipeByFormat(Format);
          
          ValidationResult := TJSONObject.Create;
          try
            ReverseResult := Pipe.ReverseTransform(InputData, TPerson);
            
            ValidationResult.AddPair('valid', ReverseResult.Success);
            ValidationResult.AddPair('format', Format);
            
            if ReverseResult.Success then
            begin
              ValidationResult.AddPair('message', 'Valid ' + Format.ToUpper + ' format');
              ReverseResult.Data.AsObject.Free;
            end
            else
            begin
              ValidationResult.AddPair('error', ReverseResult.ErrorMessage);
            end;
            
            Res.Send(ValidationResult.ToString);
          finally
            ValidationResult.Free;
          end;
        end)
      
      .Listen(8080);
    
    WriteLn('🚀 Plugin Transform Server rodando em http://localhost:8080');
    WriteLn('');
    WriteLn('🔌 Plugins disponíveis:');
    WriteLn('  • JsonBr - JSON otimizado');
    WriteLn('  • XML - Extensible Markup Language');
    WriteLn('  • YAML - YAML Ain''t Markup Language');
    WriteLn('  • CSV - Comma Separated Values');
    WriteLn('');
    WriteLn('📡 Endpoints:');
    WriteLn('  GET    /api/plugins              - Listar plugins');
    WriteLn('  POST   /api/transform/:format    - Transformar para formato');
    WriteLn('  POST   /api/convert/:from/:to    - Converter entre formatos');
    WriteLn('  POST   /api/validate/:format     - Validar formato');
    WriteLn('');
    WriteLn('💡 Exemplos:');
    WriteLn('  curl http://localhost:8080/api/plugins');
    WriteLn('  curl -X POST http://localhost:8080/api/transform/xml');
    WriteLn('  curl -X POST http://localhost:8080/api/transform/yaml');
    WriteLn('  curl -X POST -d ''{"name":"Test"}'' http://localhost:8080/api/convert/json/xml');
    WriteLn('');
    WriteLn('Pressione ENTER para parar...');
    ReadLn;
    
  except
    on E: Exception do
    begin
      WriteLn('❌ Erro: ' + E.Message);
      ReadLn;
    end;
  end;
end.
```

## 🧪 Testando os Plugins

### 1. Listar Plugins Disponíveis

```bash
curl http://localhost:8080/api/plugins
```

**Resposta:**
```json
[
  {"format": "json", "name": "JsonBr Transform Pipe"},
  {"format": "xml", "name": "XML Transform Pipe"},
  {"format": "yaml", "name": "YAML Transform Pipe"},
  {"format": "csv", "name": "CSV Transform Pipe"}
]
```

### 2. Transformar para XML

```bash
curl -X POST http://localhost:8080/api/transform/xml
```

**Resposta:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<TPerson>
  <Name>João Silva</Name>
  <Age>30</Age>
  <Email>joao@email.com</Email>
  <Active>true</Active>
</TPerson>
```

### 3. Transformar para YAML

```bash
curl -X POST http://localhost:8080/api/transform/yaml
```

**Resposta:**
```yaml
Name: "João Silva"
Age: 30
Email: "joao@email.com"
Active: true
```

### 4. Converter JSON para CSV

```bash
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{"Name":"Maria","Age":25,"Email":"maria@email.com","Active":true}' \
  http://localhost:8080/api/convert/json/csv
```

**Resposta:**
```csv
Name,Age,Email,Active
"Maria",25,"maria@email.com",true
```

### 5. Validar Formato

```bash
curl -X POST \
  -H "Content-Type: application/xml" \
  -d '<?xml version="1.0"?><TPerson><Name>Test</Name><Age>20</Age></TPerson>' \
  http://localhost:8080/api/validate/xml
```

**Resposta:**
```json
{
  "valid": true,
  "format": "xml",
  "message": "Valid XML format"
}
```

## 🔧 Plugin Registry Avançado

### 1. Sistema de Registro Dinâmico

```pascal
// PluginRegistry.pas
unit PluginRegistry;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  nest4d.pipes.transforms;

type
  TPluginRegistry = class
  private
    class var FInstance: TPluginRegistry;
    FPipes: TDictionary<string, ITransformPipe>;
    FFormatMappings: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    
    class function Instance: TPluginRegistry;
    
    procedure RegisterPipe(const Name: string; Pipe: ITransformPipe);
    procedure RegisterFormat(const Format, PipeName: string);
    function GetPipe(const Name: string): ITransformPipe;
    function GetPipeByFormat(const Format: string): ITransformPipe;
    function GetRegisteredPipes: TArray<string>;
    function GetSupportedFormats: TArray<string>;
    procedure UnregisterPipe(const Name: string);
  end;

implementation

constructor TPluginRegistry.Create;
begin
  inherited;
  FPipes := TDictionary<string, ITransformPipe>.Create;
  FFormatMappings := TDictionary<string, string>.Create;
end;

destructor TPluginRegistry.Destroy;
begin
  FPipes.Free;
  FFormatMappings.Free;
  inherited;
end;

class function TPluginRegistry.Instance: TPluginRegistry;
begin
  if not Assigned(FInstance) then
    FInstance := TPluginRegistry.Create;
  Result := FInstance;
end;

procedure TPluginRegistry.RegisterPipe(const Name: string; Pipe: ITransformPipe);
var
  Format: string;
begin
  FPipes.AddOrSetValue(Name.ToLower, Pipe);
  
  // Auto-registrar formatos suportados
  for Format in Pipe.GetSupportedFormats do
    FFormatMappings.AddOrSetValue(Format.ToLower, Name.ToLower);
end;

procedure TPluginRegistry.RegisterFormat(const Format, PipeName: string);
begin
  FFormatMappings.AddOrSetValue(Format.ToLower, PipeName.ToLower);
end;

function TPluginRegistry.GetPipe(const Name: string): ITransformPipe;
begin
  if not FPipes.TryGetValue(Name.ToLower, Result) then
    raise Exception.CreateFmt('Plugin not found: %s', [Name]);
end;

function TPluginRegistry.GetPipeByFormat(const Format: string): ITransformPipe;
var
  PipeName: string;
begin
  if FFormatMappings.TryGetValue(Format.ToLower, PipeName) then
    Result := GetPipe(PipeName)
  else
    raise Exception.CreateFmt('No plugin found for format: %s', [Format]);
end;

function TPluginRegistry.GetRegisteredPipes: TArray<string>;
begin
  Result := FPipes.Keys.ToArray;
end;

function TPluginRegistry.GetSupportedFormats: TArray<string>;
begin
  Result := FFormatMappings.Keys.ToArray;
end;

procedure TPluginRegistry.UnregisterPipe(const Name: string);
var
  Format: string;
  FormatsToRemove: TArray<string>;
begin
  FPipes.Remove(Name.ToLower);
  
  // Remover mapeamentos de formato
  SetLength(FormatsToRemove, 0);
  for Format in FFormatMappings.Keys do
  begin
    if SameText(FFormatMappings[Format], Name) then
    begin
      SetLength(FormatsToRemove, Length(FormatsToRemove) + 1);
      FormatsToRemove[High(FormatsToRemove)] := Format;
    end;
  end;
  
  for Format in FormatsToRemove do
    FFormatMappings.Remove(Format);
end;

end.
```

## 🚀 Próximos Passos

1. **Criar Plugin Protocol Buffers**: Para serialização binária eficiente
2. **Implementar Plugin MessagePack**: Formato binário compacto
3. **Adicionar Plugin BSON**: Para integração com MongoDB
4. **Criar Plugin Avro**: Para schemas evolutivos
5. **Implementar Validação de Schema**: Validar dados contra schemas

## 📚 Recursos Relacionados

- [Exemplo Básico](./basic-server.md)
- [Exemplo com Resiliência](./resilience.md)
- [Exemplo com Microserviços](./microservices.md)
- [Guia de Arquitetura](../architecture.md)

---

O sistema de plugins Transform Pipes do Nest4D oferece uma arquitetura extensível e flexível para trabalhar com diferentes formatos de dados, mantendo o código limpo e organizado.