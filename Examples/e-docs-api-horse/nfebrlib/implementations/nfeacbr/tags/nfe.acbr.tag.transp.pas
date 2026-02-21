unit nfe.acbr.tag.transp;

interface

uses
  SysUtils,
  ACBrNFe,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
  nfebr.model,
  nfe.acbr.interfaces;

type
  TNFeTagTransp = class(TInterfacedObject, INFeExecute)
  private
    FNFe: TACBrNFe;
  public
    class function Factory(ADFe: TACBrNFe): INFeExecute;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

{ TDFeTagTransp }

constructor TNFeTagTransp.Create(ADFe: TACBrNFe);
begin
  FNFe := ADFe;
end;

destructor TNFeTagTransp.Destroy;
begin
  FNFe := nil;
  inherited;
end;

procedure TNFeTagTransp.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LTransp: TTransp;
begin
  LTransp := FNFe.NotasFiscais.Items[AIndex].NFe.Transp;
  case ANFeModel.InfNFe.Transp.ModFrete of
    0: LTransp.modFrete := mfContaEmitente;
    1: LTransp.modFrete := mfContaDestinatario;
    2: LTransp.modFrete := mfContaTerceiros;
    3: LTransp.modFrete := mfProprioRemetente;
    4: LTransp.modFrete := mfProprioDestinatario;
    9: LTransp.modFrete := mfSemFrete;
  end;
  if LTransp.modFrete = mfSemFrete then
    exit;
  LTransp.Transporta.IE := ANFeModel.InfNFe.Transp.Transporta.IE;
  LTransp.Transporta.CNPJCPF := ANFeModel.InfNFe.Transp.Transporta.CNPJ;
  LTransp.Transporta.xNome := ANFeModel.InfNFe.Transp.Transporta.XNome;
  LTransp.Transporta.xEnder := ANFeModel.InfNFe.Transp.Transporta.XEnder;
  LTransp.Transporta.xMun := ANFeModel.InfNFe.Transp.Transporta.XMun;
  LTransp.Transporta.UF := ANFeModel.InfNFe.Transp.Transporta.UF;
end;

class function TNFeTagTransp.Factory(ADFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(ADFe);
end;

end.
