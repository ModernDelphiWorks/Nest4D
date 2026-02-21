unit nfebr.tag.pisqtde.model;

interface

type
  TPISQtdeModel = class
  private
    FQBCProd: Double;
    FVAliqProd: Currency;
    FVPIS: Currency;
  public
    property QBCProd: Double read FQBCProd write FQBCProd;
    property VAliqProd: Currency read FVAliqProd write FVAliqProd;
    property VPIS: Currency read FVPIS write FVPIS;
  end;

implementation

end.

