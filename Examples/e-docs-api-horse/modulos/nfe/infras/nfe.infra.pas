unit nfe.infra;

interface

uses
  Generics.Collections,
  core.include,
  core.types,
  nfebr.lib.json,
  nfebr.lib.nfe,
  nfebr.config,
  nfebr.lib.include,
  nfebr.model,
  nfe.interfaces;

type
  TNFeInfra = class
  private
    FNFeLib: INFeLib;
    FJsonLib: TJsonLib;
    FNFeConfig: TNFeConfig;
    procedure _GerarPDF;
    procedure _SetFNFeConfig(const Value: TNFeConfig);
  public
    constructor Create(const ANFeLib: INFeLib; const AJsonLib: TJsonLib);
    destructor Destroy; override;
    function Cancelar(const AReq: TNFeRequestCancela): TNFeResponseCancela;
    function CartaCorrecao(const AReq: TNFeRequestCCe): TNFeResponseCCe;
    function Consultar(const AReq: TNFeRequestConsulta): TNFeResponseConsulta;
    function Inutilizar(const AReq: TNFeRequestInutiliza): TNFeResponseInutiliza;
    function Transmitir(const AReq: TNFeRequestTransmite): TNFeResponseTransmite;
    function TransmitirLote(const AReq: TObjectList<TNFeRequestTransmite>): TNFeResponseTransmiteLote;
    function ServicoStatus: TNFeResponseServicoStatus;
    // Json
    function FromJson<T: class, constructor>(const AJson: String): T;
    function FromJsonList<T: class, constructor>(const AJson: String): TObjectList<T>;
    function ToJson<T: class, constructor>(const AObject: T): String;
    function ToJsonList<T: class, constructor>(const AList: TObjectList<T>): String;
    //
    property NFeConfig: TNFeConfig read FNFeConfig write _SetFNFeConfig;
  end;

implementation

{ TNFeInfra }

constructor TNFeInfra.Create(const ANFeLib: INFeLib; const AJsonLib: TJsonLib);
begin
  FNFeLib := ANFeLib;
  FJsonLib := AJsonLib;
end;

destructor TNFeInfra.Destroy;
begin
  FNFeConfig := nil;
  FNFeLib := nil;
  FJsonLib := nil;
  inherited;
end;

function TNFeInfra.Cancelar(const AReq: TNFeRequestCancela): TNFeResponseCancela;
begin
  Result := FNFeLib.Cancelar(AReq);
end;

function TNFeInfra.CartaCorrecao(const AReq: TNFeRequestCCe): TNFeResponseCCe;
begin
  Result := FNFeLib.CartaCorrecao(AReq);
end;

function TNFeInfra.Consultar(const AReq: TNFeRequestConsulta): TNFeResponseConsulta;
begin
  Result := FNFeLib.Consultar(AReq);
end;

procedure TNFeInfra._GerarPDF;
begin
  FNFeLib.GerarPDF;
end;

procedure TNFeInfra._SetFNFeConfig(const Value: TNFeConfig);
begin
  FNFeConfig := Value;
  // Seta arquivo de configurações dos componentes.
  FNFeLib.SetConfigracao(FNFeConfig);
end;

function TNFeInfra.Inutilizar(const AReq: TNFeRequestInutiliza): TNFeResponseInutiliza;
begin
  Result := FNFeLib.Inutilizar(AReq);
end;

function TNFeInfra.ServicoStatus: TNFeResponseServicoStatus;
begin
  Result := FNFeLib.ServicoStatus;
end;

function TNFeInfra.Transmitir(const AReq: TNFeRequestTransmite): TNFeResponseTransmite;
begin
  Result := FNFeLib.Transmitir(AReq);
end;

function TNFeInfra.TransmitirLote(
  const AReq: TObjectList<TNFeRequestTransmite>): TNFeResponseTransmiteLote;
begin
  Result := FNFeLib.TransmitirLote(AReq);
end;

function TNFeInfra.ToJson<T>(const AObject: T): String;
begin
  Result := FJsonLib.ToJson(AObject);
end;

function TNFeInfra.ToJsonList<T>(const AList: TObjectList<T>): String;
begin
  Result := FJsonLib.ToJsonList<T>(AList);
end;

function TNFeInfra.FromJson<T>(const AJson: String): T;
begin
  Result := FJsonLib.FromJson<T>(AJson);
end;

function TNFeInfra.FromJsonList<T>(const AJson: String): TObjectList<T>;
begin
  Result := FJsonLib.FromJsonList<T>(AJson);
end;

end.
