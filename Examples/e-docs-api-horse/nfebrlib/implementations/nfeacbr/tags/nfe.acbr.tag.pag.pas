unit nfe.acbr.tag.pag;

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
  TNFeTagPag = class(TInterfacedObject, INFeExecute)
  private
    FNFe: TACBrNFe;
//    procedure SetTipoPagamentoNFe(const ADFeDataSet: IDBResultSet);
    function _GetTipoPagamento(AForma: Integer): TpcnFormaPagamento;
  public
    class function Factory(ADFe: TACBrNFe): INFeExecute;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

{ TDFeTagPag }

constructor TNFeTagPag.Create(ADFe: TACBrNFe);
begin
  FNFe := ADFe;
end;

destructor TNFeTagPag.Destroy;
begin
  FNFe := nil;
  inherited;
end;

procedure TNFeTagPag.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
begin
//  if FDFe.NotasFiscais.Items[AIndex].NFe.Ide.modelo = 55 then
//    SetTipoPagamentoNFe(ADFeDataSet)
end;

function TNFeTagPag._GetTipoPagamento(AForma: Integer): TpcnFormaPagamento;
begin
  case AForma of
    1: Result := fpDinheiro;
    2: Result := fpCheque;
    3: Result := fpCreditoLoja;
    4: Result := fpValePresente;
    5: Result := fpCartaoCredito;
    6: Result := fpValeRefeicao;
    7: Result := fpSemPagamento;
  else
    Result := fpOutro;

//    3: Result := fpCartaoDebito;
//    5: Result := fpValeAlimentacao;
//    8: Result := fpValeCombustivel;
//    9: Result := fpDuplicataMercantil;
//    10: Result := fpBoletoBancario;
  end
end;

class function TNFeTagPag.Factory(ADFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(ADFe);
end;

//procedure TNFeTagPag.SetTipoPagamentoNFe(const ADFeDataSet: IDBResultSet);
//var
//  LNFe: TNFe;
//begin
//  LNFe := FDFe.NotasFiscais.Items[AIndex].NFe;
//  try
//    LNFe.Ide.indPag := ipVista;
////    if LPagtoSet.RecordCount > 0 then
////    begin
////      with LNFe.pag.Add do
////      begin
//////        if LNFe.Ide.finNFe in [fnAjuste, fnDevolucao] then
//////        else
//////        begin
////          indPag := LNFe.Ide.indPag;
////          tPag := GetTipoPagamento(LPagtoSet.FieldByName('TIPO_CODIGO').AsInteger);
////          vPag := ADFeDataSet.FieldByName('DFE_TOTALNF').AsCurrency;
////          tpIntegra := tiPagIntegrado;
////          if tPag in [fpCartaoCredito, fpCartaoDebito] then
////            tpIntegra := tiPagNaoIntegrado
////
//////          CNPJ := '';
//////          tBand := StrToBandeiraCartao(LOK, LPagtoSet.FieldByName('TIPO_CARTAOBANDEIRA').AsString);
//////          cAut := '';
//////        end;
////      end;
////      LNFe.pag.vTroco := 0;
////    end
////    else
////    begin
//      with LNFe.pag.Add do
//      begin
//        indPag := LNFe.Ide.indPag;
//        tPag := fpSemPagamento;
//        vPag := ADFeDataSet.FieldByName('DFE_TOTALNF').AsCurrency;
//      end;
////    end;
//end;

end.
