unit nfebr.tag.det.model;

interface

uses
  nfebr.tag.imposto.model,
  nfebr.tag.impostodevol.model,
  nfebr.tag.obsitem.model,
  nfebr.tag.prod.model;


type
  TDetModel = class
  private
    FImposto: TImpostoModel;
    FImpostoDevol: TImpostoDevolModel;
    FInfAdProd: String;
    FNItem: integer;
    FObsItem: TObsItemModel;
    FProd: TProdModel;
  public
    constructor Create;
    destructor Destroy; override;
    property Imposto: TImpostoModel read FImposto;
    property ImpostoDevol: TImpostoDevolModel read FImpostoDevol;
    property InfAdProd: String read FInfAdProd write FInfAdProd;
    property NItem: integer read FNItem write FNItem;
    property ObsItem: TObsItemModel read FObsItem;
    property Prod: TProdModel read FProd;
  end;

implementation

{ TDetModel }

constructor TDetModel.Create;
begin
  FImposto := TImpostoModel.Create;
  FImpostoDevol := TImpostoDevolModel.Create;
  FObsItem := TObsItemModel.Create;
  FProd := TProdModel.Create;
end;

destructor TDetModel.Destroy;
begin
  FImposto.Free;
  FImpostoDevol.Free;
  FObsItem.Free;
  FProd.Free;
  inherited;
end;

end.

