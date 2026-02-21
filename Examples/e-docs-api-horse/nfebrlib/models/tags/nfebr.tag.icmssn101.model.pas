unit nfebr.tag.icmssn101.model;

interface

type
  TICMSSN101Model = class
  private
    FPCredSN: Double;
    FVCredICMSSN: Currency;
  public
    property PCredSN: Double read FPCredSN write FPCredSN;
    property VCredICMSSN: Currency read FVCredICMSSN write FVCredICMSSN;
  end;

implementation

end.



