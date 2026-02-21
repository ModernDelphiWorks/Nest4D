unit nfebr.tag.cide.model;

interface

type
  TCIDEModel = class
  private
    FQBCProd: Double;
    FVAliqProd: Currency;
    FVCIDE: Currency;
  public
    property QBCProd: Double read FQBCProd write FQBCProd;
    property VAliqProd: Currency read FVAliqProd write FVAliqProd;
    property VCIDE: Currency read FVCIDE write FVCIDE;
  end;

implementation

end.

