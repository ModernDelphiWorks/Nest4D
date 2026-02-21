unit nfebr.tag.nfref.model;

interface

uses
  nfebr.tag.refecf.model,
  nfebr.tag.refnf.model,
  nfebr.tag.refnfp.model;

type
  TNFrefModel = class(TObject)
  private
    FRefCTe: String;
    FRefECF: TRefECFModel;
    FRefNF: TRefNFModel;
    FRefNFP: TRefNFPModel;
    FRefNFe: String;
    FRefNFeSig: String;
  public
    constructor Create;
    destructor Destroy; override;
    property RefCTe: String read FRefCTe write FRefCTe;
    property RefECF: TRefECFModel read FRefECF;
    property RefNF: TRefNFModel read FRefNF;
    property RefNFP: TRefNFPModel read FRefNFP;
    property RefNFe: String read FRefNFe write FRefNFe;
    property RefNFeSig: String read FRefNFeSig write FRefNFeSig;
  end;

implementation

constructor TNFrefModel.Create;
begin
  FRefECF := TRefECFModel.Create;
  FRefNF := TRefNFModel.Create;
  FRefNFP := TRefNFPModel.Create;
end;

destructor TNFrefModel.Destroy;
begin
  FRefECF.Free;
  FRefNF.Free;
  FRefNFP.Free;
  inherited;
end;

end.
