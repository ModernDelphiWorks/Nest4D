unit nfe.service;

interface

uses
  SysUtils,
  core.types,
  core.exception,
  nfe.repository,
  nfe.interfaces;

type
  TNFeService = class(TInterfacedObject, INFe)
  private
    FRepository: TNFeRepository;
  public
    constructor Create(const ARepository: TNFeRepository);
    destructor Destroy; override;
    function NFeTransmitir(const AJson: String): TNFeResponse;
    function NFeTransmitirLote(const AJson: String): TNFeResponse;
    function NFeConsultar(const AJson: String): TNFeResponse;
    function NFeCancelar(const AJson: String): TNFeResponse;
    function NFeInutilizar(const AJson: String): TNFeResponse;
    function NFeCorrigir(const AJson: String): TNFeResponse;
    function NFeServicoStatus: TNFeResponse;
  end;

implementation

{ TNFeService }

constructor TNFeService.Create(const ARepository: TNFeRepository);
begin
  FRepository := ARepository;
end;

destructor TNFeService.Destroy;
begin
  FRepository.Free;
  inherited;
end;

function TNFeService.NFeTransmitir(const AJson: String): TNFeResponse;
begin
  try
    Result := TNFeResponse.Success(FRepository.NFeTransmitir(AJson));
  except
    on E: Exception do
      Result := TNFeResponse.Failure(EReqTransmiteException.Create(E.Message));
  end;
end;

function TNFeService.NFeCancelar(const AJson: String): TNFeResponse;
begin
  try
    Result := TNFeResponse.Success(FRepository.NFeCancelar(AJson));
  except
    on E: Exception do
      Result := TNFeResponse.Failure(EReqCancelaException.Create(E.Message));
  end;
end;

function TNFeService.NFeCorrigir(const AJson: String): TNFeResponse;
begin
  try
    Result := TNFeResponse.Success(FRepository.NFeCorrigir(AJson));
  except
    on E: Exception do
      Result := TNFeResponse.Failure(EReqCCeException.Create(E.Message));
  end;
end;

function TNFeService.NFeConsultar(const AJson: String): TNFeResponse;
begin
  try
    Result := TNFeResponse.Success(FRepository.NFeConsultar(AJson));
  except
    on E: Exception do
      Result := TNFeResponse.Failure(EReqCancultaException.Create(E.Message));
  end;
end;

function TNFeService.NFeInutilizar(const AJson: String): TNFeResponse;
begin
  try
    Result := TNFeResponse.Success(FRepository.NFeInutilizar(AJson));
  except
    on E: Exception do
      Result := TNFeResponse.Failure(EReqInutilizaException.Create(E.Message));
  end;
end;

function TNFeService.NFeTransmitirLote(const AJson: String): TNFeResponse;
begin
  try
    Result := TNFeResponse.Success(FRepository.NFeTransmitirLote(AJson));
  except
    on E: Exception do
      Result := TNFeResponse.Failure(EReqTransmiteLoteException.Create(E.Message));
  end;
end;

function TNFeService.NFeServicoStatus: TNFeResponse;
begin
  try
    Result := TNFeResponse.Success(FRepository.NFeServicoStatus);
  except
    on E: Exception do
      Result := TNFeResponse.Failure(EReqServicoStatuException.Create(E.Message));
  end;
end;

end.
