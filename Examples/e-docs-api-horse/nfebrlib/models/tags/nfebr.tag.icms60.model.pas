unit nfebr.tag.icms60.model;

interface

type
  TICMS60Model = class
  private
    FPFCPSTRet: Double;
    FPICMSEfet: Double;
    FPRedBCEfet: Double;
    FPST: Double;
    FVBCEfet: Currency;
    FVBCFCPSTRet: Currency;
    FVBCSTRet: Currency;
    FVFCPSTRet: Currency;
    FVICMSEfet: Currency;
    FVICMSSTRet: Currency;
    FVICMSSubstituto: Currency;
  public
    property PFCPSTRet: Double read FPFCPSTRet write FPFCPSTRet;
    property PICMSEfet: Double read FPICMSEfet write FPICMSEfet;
    property PRedBCEfet: Double read FPRedBCEfet write FPRedBCEfet;
    property PST: Double read FPST write FPST;
    property VBCEfet: Currency read FVBCEfet write FVBCEfet;
    property VBCFCPSTRet: Currency read FVBCFCPSTRet write FVBCFCPSTRet;
    property VBCSTRet: Currency read FVBCSTRet write FVBCSTRet;
    property VFCPSTRet: Currency read FVFCPSTRet write FVFCPSTRet;
    property VICMSEfet: Currency read FVICMSEfet write FVICMSEfet;
    property VICMSSTRet: Currency read FVICMSSTRet write FVICMSSTRet;
    property VICMSSubstituto: Currency read FVICMSSubstituto write FVICMSSubstituto;
  end;

implementation

end.

