unit nfebr.tag.infnfe.model;

interface
uses
  Generics.Collections,
  nfebr.tag.autxml.model,
  nfebr.tag.avulsa.model,
  nfebr.tag.cana.model,
  nfebr.tag.cobr.model,
  nfebr.tag.compra.model,
  nfebr.tag.dest.model,
  nfebr.tag.det.model,
  nfebr.tag.emit.model,
  nfebr.tag.entrega.model,
  nfebr.tag.exporta.model,
  nfebr.tag.ide.model,
  nfebr.tag.infadic.model,
  nfebr.tag.infintermed.model,
  nfebr.tag.infresptec.model,
  nfebr.tag.infsolicnff.model,
  nfebr.tag.pag.model,
  nfebr.tag.retirada.model,
  nfebr.tag.total.model,
  nfebr.tag.transp.model;

type
  TInfNFeModel = class
  private
    FId: String;
    FVersao: String;
    FAvulsa: TAvulsaModel;
    FCana: TCanaModel;
    FCobr: TCobrModel;
    FCompra: TCompraModel;
    FDest: TDestModel;
    FEmit: TEmitModel;
    FEntrega: TEntregaModel;
    FExporta: TExportaModel;
    FIde: TIdeModel;
    FInfAdic: TInfAdicModel;
    FInfIntermed: TInfIntermedModel;
    FInfRespTec: TInfRespTecModel;
    FInfSolicNFF: TInfSolicNFFModel;
    FPag: TPagModel;
    FRetirada: TRetiradaModel;
    FTotal: TTotalModel;
    FTransp: TTranspModel;
    FAutXML: TObjectList<TAutXMLModel>;
    FDet: TObjectList<TDetModel>;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: String read FId write FId;
    property Versao: String read FVersao write FVersao;
    property Avulsa: TAvulsaModel read FAvulsa;
    property Cana: TCanaModel read FCana;
    property Cobr: TCobrModel read FCobr;
    property Compra: TCompraModel read FCompra;
    property Dest: TDestModel read FDest;
    property Emit: TEmitModel read FEmit;
    property Entrega: TEntregaModel read FEntrega;
    property Exporta: TExportaModel read FExporta;
    property Ide: TIdeModel read FIde;
    property InfAdic: TInfAdicModel read FInfAdic;
    property InfIntermed: TInfIntermedModel read FInfIntermed;
    property InfRespTec: TInfRespTecModel read FInfRespTec;
    property InfSolicNFF: TInfSolicNFFModel read FInfSolicNFF;
    property Pag: TPagModel read FPag;
    property Retirada: TRetiradaModel read FRetirada;
    property Total: TTotalModel read FTotal;
    property Transp: TTranspModel read FTransp;
    property AutXML: TObjectList<TAutXMLModel> read FAutXML;
    property Det: TObjectList<TDetModel> read FDet;
  end;

implementation

{ TInfNFeModel }

constructor TInfNFeModel.Create;
begin
  inherited;
  FAutXML := TObjectList<TAutXMLModel>.Create;
  FAvulsa := TAvulsaModel.Create;
  FCana := TCanaModel.Create;
  FCobr := TCobrModel.Create;
  FCompra := TCompraModel.Create;
  FDest := TDestModel.Create;
  FDet := TObjectList<TDetModel>.Create;
  FEmit := TEmitModel.Create;
  FEntrega := TEntregaModel.Create;
  FExporta := TExportaModel.Create;
  FIde := TIdeModel.Create;
  FInfAdic := TInfAdicModel.Create;
  FInfIntermed := TInfIntermedModel.Create;
  FInfRespTec := TInfRespTecModel.Create;
  FInfSolicNFF := TInfSolicNFFModel.Create;
  FPag := TPagModel.Create;
  FRetirada := TRetiradaModel.Create;
  FTotal := TTotalModel.Create;
  FTransp := TTranspModel.Create;
end;

destructor TInfNFeModel.Destroy;
begin
  FAutXML.Free;
  FAvulsa.Free;
  FCana.Free;
  FCobr.Free;
  FCompra.Free;
  FDest.Free;
  FDet.Free;
  FEmit.Free;
  FEntrega.Free;
  FExporta.Free;
  FIde.Free;
  FInfAdic.Free;
  FInfIntermed.Free;
  FInfRespTec.Free;
  FInfSolicNFF.Free;
  FPag.Free;
  FRetirada.Free;
  FTotal.Free;
  FTransp.Free;
  inherited;
end;

end.

