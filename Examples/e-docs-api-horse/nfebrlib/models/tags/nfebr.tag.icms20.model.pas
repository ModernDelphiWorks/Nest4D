unit nfebr.tag.icms20.model;

interface

type
  TICMS20Model = class
  private
    FModBC: integer;
    FMotDesICMS: integer;
    FPFCP: Double;
    FPICMS: Double;
    FPRedBC: Double;
    FVBC: Currency;
    FVBCFCP: Currency;
    FVFCP: Currency;
    FVICMS: Currency;
    FVICMSDeson: Currency;
  public
    property ModBC: integer read FModBC write FModBC;
    property PFCP: Double read FPFCP write FPFCP;
    property PICMS: Double read FPICMS write FPICMS;
    property PRedBC: Double read FPRedBC write FPRedBC;
    property VBC: Currency read FVBC write FVBC;
    property VBCFCP: Currency read FVBCFCP write FVBCFCP;
    property VFCP: Currency read FVFCP write FVFCP;
    property VICMS: Currency read FVICMS write FVICMS;
    property MotDesICMS: integer read FMotDesICMS write FMotDesICMS;
    property VICMSDeson: Currency read FVICMSDeson write FVICMSDeson;
  end;


implementation

end.

