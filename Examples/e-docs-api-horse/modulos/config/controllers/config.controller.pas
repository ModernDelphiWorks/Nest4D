unit config.controller;

interface

uses
  core.include,
  core.types,
  config.service,
  config.interfaces;

type
  TConfigController = class(TInterfacedObject, IConfig)
  private
    FService: TConfigService;
  public
    constructor Create(const AReq: TConfigService);
    destructor Destroy; override;
    function Find: TConfigResponse;
    function Insert(const AJson: String): TConfigResponse;
    function Update(const AJson: String): TConfigResponse;
    function Delete: TConfigResponse;
  end;

implementation

uses
  Nest4D.horse;

{ TConfigController }

destructor TConfigController.Destroy;
begin
  FService.Free;
  inherited;
end;

constructor TConfigController.Create(const AReq: TConfigService);
begin
  FService := AReq;
end;

function TConfigController.Delete: TConfigResponse;
begin
  Result := FService.Delete;
end;

function TConfigController.Find: TConfigResponse;
begin
  Result := FService.Find;
end;

function TConfigController.Insert(const AJson: String): TConfigResponse;
begin
  Result := FService.Insert(AJson);
end;

function TConfigController.Update(const AJson: String): TConfigResponse;
begin
  Result := FService.Update(AJson);
end;

end.

