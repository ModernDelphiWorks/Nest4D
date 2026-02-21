unit nfebr.tag.icms40.model;

interface

type
  TICMS40Model = class
  private
    FMotDesICMS: integer;
    FVICMSDeson: Currency;
  public
    property MotDesICMS: integer read FMotDesICMS write FMotDesICMS;
    property VICMSDeson: Currency read FVICMSDeson write FVICMSDeson;
  end;

implementation

end.

