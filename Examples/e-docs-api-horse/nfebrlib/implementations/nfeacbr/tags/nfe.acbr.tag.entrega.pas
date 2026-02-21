unit nfe.acbr.tag.entrega;

interface

uses
  SysUtils,
  StrUtils,
  ACBrNFe,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
  nfebr.lib.enum,
  nfebr.model,
  nfe.acbr.interfaces;

type
  TNFeTagEntrega = class(TInterfacedObject, INFeExecute)
  private
    FACBrNFe: TACBrNFe;
  public
    class function Factory(ADFe: TACBrNFe): INFeExecute;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

{ TDFeTagEntrega }

constructor TNFeTagEntrega.Create(ADFe: TACBrNFe);
begin
  FACBrNFe := ADFe;
end;

destructor TNFeTagEntrega.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

procedure TNFeTagEntrega.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LNFe: TNFe;
begin
  LNFe := FACBrNFe.NotasFiscais.Items[AIndex].NFe;
  LNFe.Entrega.CNPJCPF := ANFeModel.InfNFe.Entrega.CNPJCPF;
  LNFe.Entrega.xLgr := ANFeModel.InfNFe.Entrega.XLgr;
  LNFe.Entrega.nro := ANFeModel.InfNFe.Entrega.Nro;
  LNFe.Entrega.xCpl := ANFeModel.InfNFe.Entrega.XCpl;
  LNFe.Entrega.xBairro := ANFeModel.InfNFe.Entrega.XBairro;
  LNFe.Entrega.cMun := ANFeModel.InfNFe.Entrega.CMun;
  LNFe.Entrega.xMun := ANFeModel.InfNFe.Entrega.XMun;
  LNFe.Entrega.UF := ANFeModel.InfNFe.Entrega.UF;
end;

class function TNFeTagEntrega.Factory(ADFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(ADFe);
end;

end.
