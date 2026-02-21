unit nfebr.tag.ii.model;

interface

type
  TIIModel = class
  private
    FVBC: Currency;
    FVDespAdu: Currency;
    FVII: Currency;
    FVIOF: Currency;
  public
    property VBC: Currency read FVBC write FVBC;
    property VDespAdu: Currency read FVDespAdu write FVDespAdu;
    property VII: Currency read FVII write FVII;
    property VIOF: Currency read FVIOF write FVIOF;
  end;

implementation

end.

