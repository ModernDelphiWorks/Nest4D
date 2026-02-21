unit nfebr.tag.pisaliq.model;

interface

type
  TPISAliqModel = class
  private
    FPPIS: Double;
    FVBC: Currency;
    FVPIS: Currency;
  public
    property PPIS: Double read FPPIS write FPPIS;
    property VBC: Currency read FVBC write FVBC;
    property VPIS: Currency read FVPIS write FVPIS;
  end;

implementation

end.

