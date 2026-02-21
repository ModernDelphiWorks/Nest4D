unit xml.module;

interface

uses
  Generics.Collections,
  nfebr.lib.json,
  Nest4D.module,
  nest4d;

type
  TXMLModule = class(TModule)
  public
    constructor Create; override;
    function Binds: TBinds; override;
    function Imports: TImports; override;
  end;

implementation

uses
  nfebr.lib.nfe,
  nfebr.lib.include,
  xml.service,
  xml.repository,
  xml.infra,
  xml.controller,
  core.module;

{ TNFeModule }

function TXMLModule.Binds: TBinds;
begin
  Result := [Bind<TXMLInfra>.Factory,
             Bind<TXMLRepository>.Factory,
             Bind<TXMLService>.Factory,
             Bind<TXMLController>.Singleton];
end;

constructor TXMLModule.Create;
begin
  inherited;

end;

function TXMLModule.Imports: TImports;
begin
  Result := [TCoreModule];
end;

end.

