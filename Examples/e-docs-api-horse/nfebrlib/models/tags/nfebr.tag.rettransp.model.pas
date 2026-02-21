unit nfebr.tag.rettransp.model;

interface

type
  TRetTranspModel = class
  private
    FCFOP: String;
    FCMunFG: String;
    FPICMSRet: Double;
    FVBCRet: Currency;
    FVICMSRet: Currency;
    FVServ: Currency;
  public
    property CFOP: String read FCFOP write FCFOP;
    property CMunFG: String read FCMunFG write FCMunFG;
    property PICMSRet: Double read FPICMSRet write FPICMSRet;
    property VBCRet: Currency read FVBCRet write FVBCRet;
    property VICMSRet: Currency read FVICMSRet write FVICMSRet;
    property VServ: Currency read FVServ write FVServ;
  end;


implementation

end.

