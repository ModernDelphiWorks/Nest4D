unit core.module;

interface

uses
  Nest4D.module,
  nfebr.model,
  nfebr.lib.nfe;

type
  TCoreModule = class(TModule)
  public
    function ExportedBinds: TExportedBinds; override;
  end;

implementation

uses
  // nfebr.lib.acbr, // Comentado temporariamente - dependências ACBr não disponíveis
  nfebr.lib.json;

{ TExportInfraModule }

function TCoreModule.ExportedBinds: TExportedBinds;
begin
  Result := [Bind<TJsonLib>.Singleton];
             // Bind<TNFeLibACBr>.SingletonInterface<INFeLib>]; // Comentado temporariamente - dependências ACBr não disponíveis
end;

end.

