unit nfebr.tag.pisst.model;

interface

type
  TPISSTModel = class
  private
    FIndSomaPISST: integer;
    FPPIS: Double;
    FQBCProd: Double;
    FVAliqProd: Currency;
    FVBC: Currency;
    FVPIS: Currency;
  public
    property IndSomaPISST: integer read FIndSomaPISST write FIndSomaPISST;
    property PPIS: Double read FPPIS write FPPIS;
    property QBCProd: Double read FQBCProd write FQBCProd;
    property VAliqProd: Currency read FVAliqProd write FVAliqProd;
    property VBC: Currency read FVBC write FVBC;
    property VPIS: Currency read FVPIS write FVPIS;
  end;

implementation

end.

