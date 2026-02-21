unit nfebr.tag.fat.model;

interface

type
  TFatModel = class
  private
    FNFat: String;
    FVDesc: Currency;
    FVLiq: Currency;
    FVOrig: Currency;
  public
    property NFat: String read FNFat write FNFat;
    property VDesc: Currency read FVDesc write FVDesc;
    property VLiq: Currency read FVLiq write FVLiq;
    property VOrig: Currency read FVOrig write FVOrig;
  end;

implementation

end.

