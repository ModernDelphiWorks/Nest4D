unit nfebr.tag.transp.model;

interface

uses
  Classes,
  Generics.Collections,
  nfebr.tag.reboque.model,
  nfebr.tag.rettransp.model,
  nfebr.tag.transporta.model,
  nfebr.tag.veictransp.model,
  nfebr.tag.vol.model;

type
  TTranspModel = class
  private
    FBalsa: String;
    FModFrete: integer;
    FVagao: String;
    FRetTransp: TRetTranspModel;
    FTransporta: TTransportaModel;
    FVeicTransp: TVeicTranspModel;
    FVol: TObjectList<TVolModel>;
    FReboque: TObjectList<TReboqueModel>;
  public
    constructor Create;
    destructor Destroy; override;
    property Balsa: String read FBalsa write FBalsa;
    property ModFrete: integer read FModFrete write FModFrete;
    property Vagao: String read FVagao write FVagao;
    property RetTransp: TRetTranspModel read FRetTransp;
    property Transporta: TTransportaModel read FTransporta;
    property VeicTransp: TVeicTranspModel read FVeicTransp;
    property Reboque: TObjectList<TReboqueModel> read FReboque write FReboque;
    property Vol: TObjectList<TVolModel> read FVol write FVol;
  end;

implementation

{ TTranspModel }

constructor TTranspModel.Create;
begin
  inherited;
  FReboque := TObjectList<TReboqueModel>.Create;
  FTransporta := TTransportaModel.Create;
  FVeicTransp := TVeicTranspModel.Create;
  FVol := TObjectList<TVolModel>.Create;
end;

destructor TTranspModel.Destroy;
begin
  FReboque.Free;
  FTransporta.Free;
  FVeicTransp.Free;
  FVol.Free;
  inherited;
end;

end.
