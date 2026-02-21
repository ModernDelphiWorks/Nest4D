unit nfe.acbr.tag.retirada;

interface

uses
  SysUtils,
  ACBrNFe,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
  nfebr.lib.enum,
  nfebr.model,
  nfe.acbr.interfaces;

type
  TNFeTagRetirada = class(TInterfacedObject, INFeExecute)
  private
    FACBrNFe: TACBrNFe;
  public
    class function Factory(AACBrNFe: TACBrNFe): INFeExecute;
    constructor Create(AACBrNFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

{ TDFeTagRetirada }

constructor TNFeTagRetirada.Create(AACBrNFe: TACBrNFe);
begin
  FACBrNFe := AACBrNFe;
end;

destructor TNFeTagRetirada.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

procedure TNFeTagRetirada.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LNFe: TNFe;
begin
  LNFe := FACBrNFe.NotasFiscais.Items[AIndex].NFe;
  if LNFe.Ide.modelo = 65 then
    Exit;
  LNfe.Retirada.CNPJCPF := ANFeModel.InfNFe.Retirada.CNPJ;
  LNfe.Retirada.xLgr := ANFeModel.InfNFe.Retirada.XLgr;
  LNfe.Retirada.nro := ANFeModel.InfNFe.Retirada.Nro;
  LNfe.Retirada.xCpl := ANFeModel.InfNFe.Retirada.XCpl;
  LNfe.Retirada.xBairro := ANFeModel.InfNFe.Retirada.XBairro;
  LNfe.Retirada.cMun := ANFeModel.InfNFe.Retirada.CMun;
  LNfe.Retirada.xMun := ANFeModel.InfNFe.Retirada.XMun;
  LNfe.Retirada.UF := ANFeModel.InfNFe.Retirada.UF;
end;

class function TNFeTagRetirada.Factory(AACBrNFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(AACBrNFe);
end;

end.
