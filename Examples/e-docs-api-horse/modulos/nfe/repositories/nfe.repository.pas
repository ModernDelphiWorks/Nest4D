unit nfe.repository;

interface

uses
  SysUtils,
  Generics.Collections,
  core.types,
  nfebr.lib.include,
  nfe.infra,
  nfe.interfaces;

type
  TNFeRepository = class
  private
    FInfra: TNFeInfra;
    FConfig: TNFeConfig;
  public
    constructor Create(const AInfra: TNFeInfra);
    destructor Destroy; override;
    function NFeTransmitir(const AJson: String): String;
    function NFeTransmitirLote(const AJson: String): String;
    function NFeConsultar(const AJson: String): String;
    function NFeCancelar(const AJson: String): String;
    function NFeInutilizar(const AJson: String): String;
    function NFeCorrigir(const AJson: String): String;
    function NFeServicoStatus: String;
  end;

implementation

uses
  nest4d;

{ TRepositoryServer }

constructor TNFeRepository.Create(const AInfra: TNFeInfra);
begin
  FConfig := GetNest4D.Get<TNFeConfig>;
  FInfra := AInfra;
  FInfra.NFeConfig := FConfig;
end;

destructor TNFeRepository.Destroy;
begin
  FConfig := nil;
  FInfra.Free;
  inherited;
end;

function TNFeRepository.NFeTransmitir(const AJson: String): String;
var
  LRequest: TNFeRequestTransmite;
  LResponse: TNFeResponseTransmite;
begin
  LRequest := nil;
  LResponse := nil;
  try
    LRequest := FInfra.FromJson<TNFeRequestTransmite>(Ajson);
    LResponse := FInfra.Transmitir(LRequest);
    Result := FInfra.ToJson<TNFeResponseTransmite>(LResponse);
  finally
    if Assigned(LRequest) then LRequest.Free;
    if Assigned(LResponse) then LResponse.Free;
  end;
end;

function TNFeRepository.NFeCancelar(const AJson: String): String;
var
  LRequest: TNFeRequestCancela;
  LResponse: TNFeResponseCancela;
begin
  LRequest := nil;
  LResponse := nil;
  try
    LRequest := FInfra.FromJson<TNFeRequestCancela>(AJson);
    LResponse := FInfra.Cancelar(LRequest);
    Result := FInfra.ToJson<TNFeResponseCancela>(LResponse);
  finally
    if Assigned(LRequest) then LRequest.Free;
    if Assigned(LResponse) then LResponse.Free;
  end;
end;

function TNFeRepository.NFeCorrigir(const AJson: String): String;
var
  LRequest: TNFeRequestCCe;
  LResponse: TNFeResponseCCe;
begin
  LRequest := nil;
  LResponse := nil;
  try
    LRequest := FInfra.FromJson<TNFeRequestCCe>(AJson);
    LResponse := FInfra.CartaCorrecao(LRequest);
    Result := FInfra.ToJson<TNFeResponseCCe>(LResponse);
  finally
    if Assigned(LRequest) then LRequest.Free;
    if Assigned(LResponse) then LResponse.Free;
  end;
end;

function TNFeRepository.NFeConsultar(const AJson: String): String;
var
  LRequest: TNFeRequestConsulta;
  LResponse: TNFeResponseConsulta;
begin
  LRequest := nil;
  LResponse := nil;
  try
    LRequest := FInfra.FromJson<TNFeRequestConsulta>(AJson);
    LResponse := FInfra.Consultar(LRequest);
    Result := FInfra.ToJson<TNFeResponseConsulta>(LResponse);
  finally
    if Assigned(LRequest) then LRequest.Free;
    if Assigned(LResponse) then LResponse.Free;
  end;
end;

function TNFeRepository.NFeInutilizar(const AJson: String): String;
var
  LRequest: TNFeRequestInutiliza;
  LResponse: TNFeResponseInutiliza;
begin
  LRequest := nil;
  LResponse := nil;
  try
    LRequest := FInfra.FromJson<TNFeRequestInutiliza>(AJson);
    LResponse := FInfra.Inutilizar(LRequest);
    Result := FInfra.ToJson<TNFeResponseInutiliza>(LResponse);
  finally
    if Assigned(LRequest) then LRequest.Free;
    if Assigned(LResponse) then LResponse.Free;
  end;
end;

function TNFeRepository.NFeTransmitirLote(const AJson: String): String;
var
  LRequest: TObjectList<TNFeRequestTransmite>;
  LResponse: TNFeResponseTransmiteLote;
begin
  LRequest := nil;
  LResponse := nil;
  try
    LRequest := FInfra.FromJsonList<TNFeRequestTransmite>(AJson);
    LResponse := FInfra.TransmitirLote(LRequest);
    Result := FInfra.ToJson<TNFeResponseTransmiteLote>(LResponse);
  finally
    if Assigned(LRequest) then LRequest.Free;
    if Assigned(LResponse) then LResponse.Free;
  end;
end;

function TNFeRepository.NFeServicoStatus: String;
var
  LResponse: TNFeResponseServicoStatus;
begin
  LResponse := nil;
  try
    LResponse := FInfra.ServicoStatus;
    Result := FInfra.ToJson<TNFeResponseServicoStatus>(LResponse);
  finally
    if Assigned(LResponse) then LResponse.Free;
  end;
end;

end.

