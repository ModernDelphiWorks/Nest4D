unit nfebr.tag.cofinsst.model;

interface

type
  TCOFINSSTModel = class
  private
    FIndSomaCOFINSST: integer;
    FPCOFINS: Double;
    FQBCProd: Double;
    FVAliqProd: Currency;
    FVBC: Currency;
    FVCOFINS: Currency;
  public
    property IndSomaCOFINSST: integer read FIndSomaCOFINSST write FIndSomaCOFINSST;
    property PCOFINS: Double read FPCOFINS write FPCOFINS;
    property QBCProd: Double read FQBCProd write FQBCProd;
    property VAliqProd: Currency read FVAliqProd write FVAliqProd;
    property VBC: Currency read FVBC write FVBC;
    property VCOFINS: Currency read FVCOFINS write FVCOFINS;
  end;

implementation

end.

