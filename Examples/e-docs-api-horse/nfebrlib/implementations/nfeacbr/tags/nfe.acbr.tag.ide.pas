unit nfe.acbr.tag.ide;

interface

uses
  SysUtils,
  System.StrUtils,
  ACBrNFe,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
  nfebr.lib.enum,
  nfe.acbr.interfaces,
  nfebr.model,
  nfebr.tag.emit.model,
  nfebr.tag.ide.model;

type
  TNFeTagIde = class(TInterfacedObject, INFeExecute)
  private
    FNFe: TACBrNFe;
  public
    class function Factory(ANFe: TACBrNFe): INFeExecute;
    constructor Create(ANFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

uses
  nfebr.lib.utils;

{ TDFeTagIde }

constructor TNFeTagIde.Create(ANFe: TACBrNFe);
begin
  FNFe := ANFe;
end;

destructor TNFeTagIde.Destroy;
begin
  FNFe := nil;
  inherited;
end;

procedure TNFeTagIde.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LNFe: TNFe;
  LIdeModel: TIdeModel;
begin
  LNFe := FNFe.NotasFiscais.Items[AIndex].nfe;
  LIdeModel := ANFeModel.InfNFe.ide;

  LNFe.ide.cUF := FNFe.Configuracoes.WebServices.UFCodigo;
  LNFe.ide.modelo := LIdeModel.modelo;
  LNFe.ide.serie := LIdeModel.serie;
  LNFe.ide.nNF := LIdeModel.nNF;
  LNFe.ide.cNF := TUtils.GenerateUniqueNumber(LNFe.ide.dEmi,
                                              LNFe.ide.nNF,
                                              LNFe.ide.serie,
                                              LNFe.ide.modelo,
                                              LNFe.ide.cUF);
  LNFe.ide.dEmi := LIdeModel.DhEmi;
  LNFe.ide.dSaiEnt := LIdeModel.DhSaiEnt;
  LNFe.ide.cMunFG := StrToIntDef(LIdeModel.cMunFG, 0);
  LNFe.ide.verProc := LIdeModel.verProc;
  LNFe.ide.tpNF := TpcnTipoNFe(LIdeModel.tpNF);
  // Tipo do Documento Fiscal (*)
  // (0)=tnEntrada
  // (1)=tnSaida
  LNFe.ide.tpImp := TpcnTipoImpressao(LIdeModel.tpImp);
  // Formato de Impressão do DANFE (*)
  // (1)=tiRetrato
  // (2)=tiPaisagem
  LNFe.ide.tpEmis := TpcnTipoEmissao(LIdeModel.tpEmis);
  // Forma de Emissão da NF-e (*)
  // (1)=teNormal
  // (2)=teContingencia
  // (3)=teSCAN
  // (4)=teDPEC
  // (5)=teFSDA
  // (6)=teSVCAN
  // (7)=teSVCRS
  // (8)=teSVCSP
  LNFe.ide.finNFe := TpcnFinalidadeNFe(LIdeModel.finNFe);
  // Finalidade de emissão da NF-e (*)
  // (1)=fnNormal
  // (2)=fnComplementar
  // (3)=fnAjuste
  // (4)=fnDevolucao
  LNFe.ide.procEmi := TpcnProcessoEmissao(LIdeModel.procEmi);
  // Processo de emissão da NF-e (*)
  // (0)=peAplicativoContribuinte      - emissão de NF-e com aplicativo do contribuinte;
  // (1)=peAvulsaFisco                 - emissão de NF-e avulsa pelo Fisco;
  // (2)=peAvulsaContribuinte          - emissão de NF-e avulsa, pelo contribuinte com seu certificado digital, através do site do Fisco;
  // (3)=peContribuinteAplicativoFisco - emissão NF-e pelo contribuinte com aplicativo fornecido pelo Fisco.
  LNFe.ide.tpAmb := TpcnTipoAmbiente(LIdeModel.tpAmb);
  // Identificação do Ambiente (*)
  // (1)=Produção
  // (2)=Homologação
  LNFe.ide.natOp := LIdeModel.natOp;
  LNFe.ide.indPres := TpcnPresencaComprador(LIdeModel.indPres);
  LNFe.ide.idDest := TpcnDestinoOperacao(LIdeModel.idDest);
end;

class function TNFeTagIde.Factory(ANFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(ANFe);
end;

end.
