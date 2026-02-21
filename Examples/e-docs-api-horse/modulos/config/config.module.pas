unit config.module;

interface

uses
  Generics.Collections,
  Nest4D.module,
  nest4d,
  config.service,
  config.repository,
  config.controller,
  config.infra,
  nfebr.lib.nfe,
  nfebr.lib.json,
  nfebr.config;

type
  TConfigModule = class(TModule)
  public
    constructor Create; override;
    function Binds: TBinds; override;
    function Imports: TImports; override;
  end;

implementation

uses
  core.module;

{ TEmpresaModule }

function TConfigModule.Binds: TBinds;
begin
  Result := [Bind<TConfigInfra>.Factory,
             Bind<TConfigRepository>.Factory,
             Bind<TConfigService>.Factory,
             Bind<TConfigController>.Singleton];
end;

constructor TConfigModule.Create;
begin
  inherited;

end;

function TConfigModule.Imports: TImports;
begin
  Result := [TCoreModule];
end;

end.

