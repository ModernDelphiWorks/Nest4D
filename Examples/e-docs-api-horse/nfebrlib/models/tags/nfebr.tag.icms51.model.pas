unit nfebr.tag.icms51.model;

interface

type
  TICMS51Model = class
  private
    FModBC: integer;
    FPDif: Double;
    FPFCP: Double;
    FPFCPDif: Double;
    FPICMS: Double;
    FPRedBC: Double;
    FVBC: Currency;
    FVBCFCP: Currency;
    FVFCP: Currency;
    FVFCPDif: Currency;
    FVFCPEfet: Currency;
    FVICMS: Currency;
    FVICMSDif: Currency;
    FVICMSOp: Currency;
  public
    property ModBC: integer read FModBC write FModBC;
    property PDif: Double read FPDif write FPDif;
    property PFCP: Double read FPFCP write FPFCP;
    property PFCPDif: Double read FPFCPDif write FPFCPDif;
    property PICMS: Double read FPICMS write FPICMS;
    property PRedBC: Double read FPRedBC write FPRedBC;
    property VBC: Currency read FVBC write FVBC;
    property VBCFCP: Currency read FVBCFCP write FVBCFCP;
    property VFCP: Currency read FVFCP write FVFCP;
    property VFCPDif: Currency read FVFCPDif write FVFCPDif;
    property VFCPEfet: Currency read FVFCPEfet write FVFCPEfet;
    property VICMS: Currency read FVICMS write FVICMS;
    property VICMSDif: Currency read FVICMSDif write FVICMSDif;
    property VICMSOp: Currency read FVICMSOp write FVICMSOp;
  end;

implementation

end.

