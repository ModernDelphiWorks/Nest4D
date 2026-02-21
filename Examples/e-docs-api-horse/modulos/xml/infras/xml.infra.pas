unit xml.infra;

interface

uses
  SysUtils,
  core.types,
  nfebr.lib.json,
  nfebr.lib.nfe,
  xml.interfaces,
  Evolution4D.Std,
  nfebr.lib.include;

type
  TXMLInfra = class
  private
    FNFeLib: INFeLib;
    FJsonLib: TJsonLib;
    FNFeConfig: TNFeConfig;
    procedure _SetFNFeConfig(const Value: TNFeConfig);
  public
    constructor Create(const ANFeLib: INFeLib; const AJsonLib: TJsonLib);
    destructor Destroy; override;
    function NFeXML(const AChave: String): String;
    function NFeCancelamentoXML(const AChave: String): String;
    function NFeInutilizacaoXML(const AProtocolo: String): String;
    function NFeCartaCorrecaoXML(const AChave: String): String;
    //
    property NFeConfig: TNFeConfig read FNFeConfig write _SetFNFeConfig;
  end;

implementation

uses
  Evolution4D.Threading,
  nfebr.lib.utils;

{ TXMLInfra }

constructor TXMLInfra.Create(const ANFeLib: INFeLib; const AJsonLib: TJsonLib);
begin
  FNFeLib := ANFeLib;
  FJsonLib := AJsonLib;
end;

destructor TXMLInfra.Destroy;
begin
  FNFeLib := nil;
  FJsonLib := nil;
  inherited;
end;

function TXMLInfra.NFeCancelamentoXML(const AChave: String): String;
begin
//  FNFeLib.Cancela()
  Result := 'NFeCancelamentoXML';
end;

function TXMLInfra.NFeCartaCorrecaoXML(const AChave: String): String;
begin
//  FNFeLib.Cancela()
  Result := 'NFeCartaCorrecaoXML';
end;

function TXMLInfra.NFeInutilizacaoXML(const AProtocolo: String): String;
begin
//  FNFeLib.Cancela()
  Result := 'NFeInutilizacaoXML';
end;

function TXMLInfra.NFeXML(const AChave: String): String;
var
  LFuture: TFuture;
  LFileName: String;
begin
  LFileName := AChave + '-nfe.xml';
  LFuture := Async(function: TValue
                   begin
                     Result := TUtils.FindFile(FNFeConfig.DFePath.FileXMLSalvar, LFileName);
                   end).Await();
  if LFuture.IsOk then
    Result := LFuture.Ok<String>
  else
    raise Exception.Create(LFuture.Err);
end;

procedure TXMLInfra._SetFNFeConfig(const Value: TNFeConfig);
begin
  FNFeConfig := Value;
end;

end.
