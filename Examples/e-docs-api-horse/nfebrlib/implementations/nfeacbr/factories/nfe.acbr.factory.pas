unit nfe.acbr.factory;

interface

uses
  SysUtils,
  Generics.Collections,
  ACBrNFe,
  // Models
  nfebr.model,
  // ACBr Framework
  nfe.acbr.interfaces,
  nfe.acbr.tag.ide,
  nfe.acbr.tag.prod,
  nfe.acbr.tag.nfref,
  nfe.acbr.tag.emit,
  nfe.acbr.tag.dest,
  nfe.acbr.tag.retirada,
  nfe.acbr.tag.entrega,
  nfe.acbr.tag.total,
  nfe.acbr.tag.transp,
  nfe.acbr.tag.veictransp,
  nfe.acbr.tag.transpvol,
  nfe.acbr.tag.infadic,
  nfe.acbr.tag.resptec,
  nfe.acbr.tag.pag;

type
  TNFeFactory = class(TInterfacedObject, INFeFactory)
  private
    FNFe: TACBrNFe;
  public
    class function New(ADFe: TACBrNFe): INFeFactory;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFe: TNFeModel);
  end;

implementation

{ TNFeFactory }

constructor TNFeFactory.Create(ADFe: TACBrNFe);
begin
  FNFe := ADFe;
end;

destructor TNFeFactory.Destroy;
begin
  inherited;
end;

procedure TNFeFactory.Execute(const AIndex: integer; const ANFe: TNFeModel);
begin
  TNFeTagIde.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagNFref.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagEmit.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagDest.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagRetirada.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagEntrega.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagProd.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagTotal.Factory(FNFe).Execute(AIndex, ANFe);
  TNfeTagTransp.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagVeicTransp.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagTransVol.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagPag.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagInfAdic.Factory(FNFe).Execute(AIndex, ANFe);
  TNFeTagRespTec.Factory(FNFe).Execute(AIndex, ANFe);
end;

class function TNFeFactory.New(ADFe: TACBrNFe): INFeFactory;
begin
  Result := Self.Create(ADFe);
end;

end.
