unit nfebr.tag.imposto.model;

interface

uses
  nfebr.tag.cofins.model,
  nfebr.tag.cofinsst.model,
  nfebr.tag.icms.model,
  nfebr.tag.icmsufdest.model,
  nfebr.tag.ii.model,
  nfebr.tag.ipi.model,
  nfebr.tag.issqn.model,
  nfebr.tag.pis.model,
  nfebr.tag.pisst.model;

type
  TImpostoModel = class
  private
    FCOFINS: TCOFINSModel;
    FCOFINSST: TCOFINSSTModel;
    FICMS: TICMSModel;
    FICMSUFDest: TICMSUFDestModel;
    FII: TIIModel;
    FIPI: TIPIModel;
    FISSQN: TISSQNModel;
    FPIS: TPISModel;
    FPISST: TPISSTModel;
    FVTotTrib: Currency;
  public
    constructor Create;
    destructor Destroy; override;
    property COFINS: TCOFINSModel read FCOFINS;
    property COFINSST: TCOFINSSTModel read FCOFINSST;
    property ICMS: TICMSModel read FICMS;
    property ICMSUFDest: TICMSUFDestModel read FICMSUFDest;
    property II: TIIModel read FII;
    property IPI: TIPIModel read FIPI;
    property ISSQN: TISSQNModel read FISSQN;
    property PIS: TPISModel read FPIS;
    property PISST: TPISSTModel read FPISST;
    property VTotTrib: Currency read FVTotTrib write FVTotTrib;
  end;

implementation

constructor TImpostoModel.Create;
begin
  FCOFINS := TCOFINSModel.Create;
  FCOFINSST := TCOFINSSTModel.Create;
  FICMS := TICMSModel.Create;
  FICMSUFDest := TICMSUFDestModel.Create;
  FII := TIIModel.Create;
  FIPI := TIPIModel.Create;
  FISSQN := TISSQNModel.Create;
  FPIS := TPISModel.Create;
  FPISST := TPISSTModel.Create;
end;

destructor TImpostoModel.Destroy;
begin
  FCOFINS.Free;
  FCOFINSST.Free;
  FICMS.Free;
  FICMSUFDest.Free;
  FII.Free;
  FIPI.Free;
  FISSQN.Free;
  FPIS.Free;
  FPISST.Free;
  inherited;
end;

end.

