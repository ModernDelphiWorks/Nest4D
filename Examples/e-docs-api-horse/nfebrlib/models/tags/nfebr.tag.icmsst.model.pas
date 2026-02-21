unit nfebr.tag.icmsst.model;

interface

type
  TICMSSTModel = class
  private
    FPFCPSTRet: Double;
    FPICMSEfet: Double;
    FPRedBCEfet: Double;
    FPST: Double;
    FVBCEfet: Currency;
    FVBCFCPSTRet: Currency;
    FVBCSTDest: Currency;
    FVBCSTRet: Currency;
    FVFCPSTRet: Currency;
    FVICMSEfet: Currency;
    FVICMSSTDest: Currency;
    FVICMSSTRet: Currency;
    FVICMSSubstituto: Currency;
  public
    property PFCPSTRet: Double read FPFCPSTRet write FPFCPSTRet;
    property PICMSEfet: Double read FPICMSEfet write FPICMSEfet;
    property PRedBCEfet: Double read FPRedBCEfet write FPRedBCEfet;
    property PST: Double read FPST write FPST;
    property VBCEfet: Currency read FVBCEfet write FVBCEfet;
    property VBCFCPSTRet: Currency read FVBCFCPSTRet write FVBCFCPSTRet;
    property VBCSTDest: Currency read FVBCSTDest write FVBCSTDest;
    property VBCSTRet: Currency read FVBCSTRet write FVBCSTRet;
    property VFCPSTRet: Currency read FVFCPSTRet write FVFCPSTRet;
    property VICMSEfet: Currency read FVICMSEfet write FVICMSEfet;
    property VICMSSTDest: Currency read FVICMSSTDest write FVICMSSTDest;
    property VICMSSTRet: Currency read FVICMSSTRet write FVICMSSTRet;
    property VICMSSubstituto: Currency read FVICMSSubstituto write FVICMSSubstituto;
  end;

implementation

end.

