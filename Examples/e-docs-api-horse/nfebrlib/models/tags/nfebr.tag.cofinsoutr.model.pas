unit nfebr.tag.cofinsoutr.model;

interface

type
  TCOFINSOutrModel = class
  private
    FPCOFINS: Double;
    FQBCProd: Double;
    FVAliqProd: Currency;
    FVBC: Currency;
    FVCOFINS: Currency;
  public
    property PCOFINS: Double read FPCOFINS write FPCOFINS;
    property QBCProd: Double read FQBCProd write FQBCProd;
    property VAliqProd: Currency read FVAliqProd write FVAliqProd;
    property VBC: Currency read FVBC write FVBC;
    property VCOFINS: Currency read FVCOFINS write FVCOFINS;
  end;

implementation

end.

