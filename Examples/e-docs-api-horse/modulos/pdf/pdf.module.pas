unit pdf.module;

interface

uses
  Generics.Collections,
  nfebr.lib.json,
  Nest4D.module,
  nest4d;

type
  TPDFModule = class(TModule)
  public
    constructor Create; override;
    function Binds: TBinds; override;
    function Imports: TImports; override;
  end;

implementation

uses
  nfebr.lib.nfe,
  nfebr.lib.include,
  pdf.controller,
  pdf.service,
  pdf.repository,
  pdf.infra,
  core.module;

{ TPDFModule }

function TPDFModule.Binds: TBinds;
begin
  Result := [Bind<TPDFInfra>.Factory,
             Bind<TPDFRepository>.Factory,
             Bind<TPDFService>.Factory,
             Bind<TPDFController>.Singleton];
end;

constructor TPDFModule.Create;
begin
  inherited;

end;

function TPDFModule.Imports: TImports;
begin
  Result := [TCoreModule];
end;

end.
