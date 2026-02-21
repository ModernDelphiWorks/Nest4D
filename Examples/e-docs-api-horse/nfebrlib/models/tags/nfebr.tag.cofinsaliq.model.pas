unit nfebr.tag.cofinsaliq.model;

interface

type
  TCOFINSAliqModel = class
  private
    FPCOFINS: Double;
    FVBC: Currency;
    FVCOFINS: Currency;
  public
    property PCOFINS: Double read FPCOFINS write FPCOFINS;
    property VBC: Currency read FVBC write FVBC;
    property VCOFINS: Currency read FVCOFINS write FVCOFINS;
  end;

implementation

end.

