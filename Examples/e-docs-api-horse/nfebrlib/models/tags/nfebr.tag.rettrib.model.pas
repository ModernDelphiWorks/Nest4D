unit nfebr.tag.rettrib.model;

interface

type
  TRetTribModel = class
  private
    FVBCIRRF: Currency;
    FVBCRetPrev: Currency;
    FVIRRF: Currency;
    FVRetCOFINS: Currency;
    FVRetCSLL: Currency;
    FVRetPIS: Currency;
    FVRetPrev: Currency;
  public
    property VBCIRRF: Currency read FVBCIRRF write FVBCIRRF;
    property VBCRetPrev: Currency read FVBCRetPrev write FVBCRetPrev;
    property VIRRF: Currency read FVIRRF write FVIRRF;
    property VRetCOFINS: Currency read FVRetCOFINS write FVRetCOFINS;
    property VRetCSLL: Currency read FVRetCSLL write FVRetCSLL;
    property VRetPIS: Currency read FVRetPIS write FVRetPIS;
    property VRetPrev: Currency read FVRetPrev write FVRetPrev;
  end;

implementation

end.

