unit nfebr.tag.icms53.model;

interface

type
  TICMS53Model = class
  private
    FAdRemICMSDif: integer;
    FVICMSMonoDif: Currency;
  public
    property AdRemICMSDif: integer read FAdRemICMSDif write FAdRemICMSDif;
    property VICMSMonoDif: Currency read FVICMSMonoDif write FVICMSMonoDif;
  end;

implementation

end.

