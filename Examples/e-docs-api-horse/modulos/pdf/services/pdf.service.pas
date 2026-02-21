unit pdf.service;

interface

uses
  SysUtils,
  core.types,
  core.exception,
  pdf.repository,
  pdf.interfaces;

type
  TPDFService = class(TInterfacedObject, IPDF)
  private
    FRepository: TPDFRepository;
  public
    constructor Create(const ARepository: TPDFRepository);
    destructor Destroy; override;
    function NFePDF(const AChave: String): TPDFResponse;
    function NFeCancelamentoPDF(const AChave: String): TPDFResponse;
    function NFeInutilizacaoPDF(const AProtocolo: String): TPDFResponse;
    function NFeCartaCorrecaoPDF(const AChave: String): TPDFResponse;
  end;

implementation

{ TPDFService }

constructor TPDFService.Create(const ARepository: TPDFRepository);
begin
  FRepository := Arepository;
end;

destructor TPDFService.Destroy;
begin
  FRepository.Free;
  inherited;
end;

function TPDFService.NFeCancelamentoPDF(const AChave: String): TPDFResponse;
begin
  try
    Result := TPDFResponse.Success(FRepository.NFeCancelamentoPDF(AChave));
  except
    on E: Exception do
      Result := TPDFResponse.Failure(ERequestException.Create(E.Message));
  end;
end;

function TPDFService.NFeCartaCorrecaoPDF(const AChave: String): TPDFResponse;
begin
  try
    Result := TPDFResponse.Success(FRepository.NFeCartaCorrecaoPDF(AChave));
  except
    on E: Exception do
      Result := TPDFResponse.Failure(ERequestException.Create(E.Message));
  end;
end;

function TPDFService.NFeInutilizacaoPDF(const AProtocolo: String): TPDFResponse;
begin
  try
    Result := TPDFResponse.Success(FRepository.NFeInutilizacaoPDF(AProtocolo));
  except
    on E: Exception do
      Result := TPDFResponse.Failure(ERequestException.Create(E.Message));
  end;
end;

function TPDFService.NFePDF(const AChave: String): TPDFResponse;
begin
  try
    Result := TPDFResponse.Success(FRepository.NFePDF(AChave));
  except
    on E: EPDFNotExistException do
      Result := TPDFResponse.Failure(ERequestException.Create(E.Message, E.Status));
  end;
end;

end.
