unit nfebr.tag.icms02.model;

interface

type
  TICMS02Model = class
  private
    FAdRemICMS: Currency;
    FVICMSMono: Currency;
  public
    property AdRemICMS: Currency read FAdRemICMS write FAdRemICMS;
    property VICMSMono: Currency read FVICMSMono write FVICMSMono;
  end;

implementation

end.

