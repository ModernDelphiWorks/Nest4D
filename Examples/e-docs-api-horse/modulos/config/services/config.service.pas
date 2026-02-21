unit config.service;

interface

uses
  SysUtils,
  core.types,
  core.exception,
  config.repository,
  config.interfaces;

type
  TConfigService = class(TInterfacedObject, IConfig)
  private
    FRepository: TConfigRepository;
  public
    constructor Create(const ARepository: TConfigRepository);
    destructor Destroy; override;
    function Find: TConfigResponse;
    function Insert(const AJson: String): TConfigResponse;
    function Update(const AJson: String): TConfigResponse;
    function Delete: TConfigResponse;
  end;

implementation

{ TConfigService }

constructor TConfigService.Create(const ARepository: TConfigRepository);
begin
  FRepository := ARepository;
end;

destructor TConfigService.Destroy;
begin
  FRepository.Free;
  inherited;
end;

function TConfigService.Delete: TConfigResponse;
begin
  try
    Result := TConfigResponse.Success(FRepository.Delete);
  except
    on E: Exception do
      Result := TConfigResponse.Failure(ERequestException.Create(E.Message));
  end;
end;

function TConfigService.Find: TConfigResponse;
begin
  try
    Result := TConfigResponse.Success(FRepository.Find);
  except
    on E: Exception do
      Result := TConfigResponse.Failure(ERequestException.Create(E.Message));
  end;
end;

function TConfigService.Insert(const AJson: String): TConfigResponse;
begin
  try
    Result := TConfigResponse.Success(FRepository.Insert(AJson));
  except
    on E: Exception do
      Result := TConfigResponse.Failure(ERequestException.Create(E.Message));
  end;
end;

function TConfigService.Update(const AJson: String): TConfigResponse;
begin
  try
    Result := TConfigResponse.Success(FRepository.Update(AJson));
  except
    on E: Exception do
      Result := TConfigResponse.Failure(ERequestException.Create(E.Message));
  end;
end;

end.

