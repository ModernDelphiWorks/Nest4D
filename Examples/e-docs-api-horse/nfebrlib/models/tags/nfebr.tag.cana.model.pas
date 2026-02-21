unit nfebr.tag.cana.model;

interface

uses
  Generics.Collections,
  nfebr.tag.deduc.model,
  nfebr.tag.fordia.model;

type
  TCanaModel = class
  private
    FQTotAnt: Double;
    FQTotGer: Double;
    FQTotMes: Double;
    FRef: String;
    FSafra: String;
    FVFor: Currency;
    FVLiqFor: Currency;
    FVTotDed: Currency;
    FDeduc: TObjectList<TDeducModel>;
    FForDia: TObjectList<TForDiaModel>;
  public
    constructor Create;
    destructor Destroy; override;
    property QTotAnt: Double read FQTotAnt write FQTotAnt;
    property QTotGer: Double read FQTotGer write FQTotGer;
    property QTotMes: Double read FQTotMes write FQTotMes;
    property Ref: String read FRef write FRef;
    property Safra: String read FSafra write FSafra;
    property VFor: Currency read FVFor write FVFor;
    property VLiqFor: Currency read FVLiqFor write FVLiqFor;
    property VTotDed: Currency read FVTotDed write FVTotDed;
    property Deduc: TObjectList<TDeducModel> read FDeduc write FDeduc;
    property ForDia: TObjectList<TForDiaModel> read FForDia write FForDia;
  end;

implementation

{ TCanaModel }

constructor TCanaModel.Create;
begin
  FDeduc := TObjectList<TDeducModel>.Create;
  FForDia := TObjectList<TForDiaModel>.Create;
end;

destructor TCanaModel.Destroy;
begin
  FDeduc.Free;
  FForDia.Free;
  inherited;
end;

end.

