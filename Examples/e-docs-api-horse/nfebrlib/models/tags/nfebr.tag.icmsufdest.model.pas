unit nfebr.tag.icmsufdest.model;

interface

type
  TICMSUFDestModel = class
  private
    FPFCPUFDest: Double;
    FPICMSInter: Double;
    FPICMSInterPart: Double;
    FPICMSUFDest: Double;
    FVBCFCPUFDest: Currency;
    FVBCUFDest: Currency;
    FVFCPUFDest: Currency;
    FVICMSUFDest: Currency;
    FVICMSUFRemet: Currency;
  public
    property PFCPUFDest: Double read FPFCPUFDest write FPFCPUFDest;
    property PICMSInter: Double read FPICMSInter write FPICMSInter;
    property PICMSInterPart: Double read FPICMSInterPart write FPICMSInterPart;
    property PICMSUFDest: Double read FPICMSUFDest write FPICMSUFDest;
    property VBCFCPUFDest: Currency read FVBCFCPUFDest write FVBCFCPUFDest;
    property VBCUFDest: Currency read FVBCUFDest write FVBCUFDest;
    property VFCPUFDest: Currency read FVFCPUFDest write FVFCPUFDest;
    property VICMSUFDest: Currency read FVICMSUFDest write FVICMSUFDest;
    property VICMSUFRemet: Currency read FVICMSUFRemet write FVICMSUFRemet;
  end;

implementation

end.

