unit nfebr.tag.pisoutr.model;

interface

type
  TPISOutrModel = class
  private
    FPPIS: Double;
    FQBCProd: Double;
    FVAliqProd: Currency;
    FVBC: Currency;
    FVPIS: Currency;
  public
    property PPIS: Double read FPPIS write FPPIS;
    property QBCProd: Double read FQBCProd write FQBCProd;
    property VAliqProd: Currency read FVAliqProd write FVAliqProd;
    property VBC: Currency read FVBC write FVBC;
    property VPIS: Currency read FVPIS write FVPIS;
  end;

implementation

end.

