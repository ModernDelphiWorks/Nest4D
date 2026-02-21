unit nfebr.tag.cofinsqtde.model;

interface

type
  TCOFINSQtdeModel = class
  private
    FQBCProd: Double;
    FVAliqProd: Currency;
    FVCOFINS: Currency;
  public
    property QBCProd: Double read FQBCProd write FQBCProd;
    property VAliqProd: Currency read FVAliqProd write FVAliqProd;
    property VCOFINS: Currency read FVCOFINS write FVCOFINS;
  end;

implementation

end.

