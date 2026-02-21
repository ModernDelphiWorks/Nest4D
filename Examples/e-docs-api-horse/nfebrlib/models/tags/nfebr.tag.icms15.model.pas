unit nfebr.tag.icms15.model;

interface

type
  TICMS15Model = class
  private
    FAdRemICMS: Currency;
    FAdRemICMSReten: Currency;
    FVICMSMono: Currency;
    FVICMSMonoReten: Currency;
  public
    property AdRemICMS: Currency read FAdRemICMS write FAdRemICMS;
    property AdRemICMSReten: Currency read FAdRemICMSReten write FAdRemICMSReten;
    property VICMSMono: Currency read FVICMSMono write FVICMSMono;
    property VICMSMonoReten: Currency read FVICMSMonoReten write FVICMSMonoReten;
  end;

implementation

end.

