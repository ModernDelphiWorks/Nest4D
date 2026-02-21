unit nfebr.tag.icmsinter.model;

interface

type
  TICMSInterModel = class
  private
    FvBCICMSSTDest: Currency;
    FvICMSSTDest: Currency;
  public
    property vBCICMSSTDest: Currency read FvBCICMSSTDest write FvBCICMSSTDest;
    property vICMSSTDest: Currency read FvICMSSTDest write FvICMSSTDest;
  end;

implementation

end.
