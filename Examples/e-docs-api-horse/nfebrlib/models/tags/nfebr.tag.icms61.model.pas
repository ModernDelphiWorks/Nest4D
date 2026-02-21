unit nfebr.tag.icms61.model;

interface

type
  TICMS61Model = class
  private
    FAdRemICMSRet: integer;
    FVICMSMonoRet: Currency;
  public
    property AdRemICMSRet: integer read FAdRemICMSRet write FAdRemICMSRet;
    property VICMSMonoRet: Currency read FVICMSMonoRet write FVICMSMonoRet;
  end;

implementation

end.

