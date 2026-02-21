unit nfebr.tag.deduc.model;

interface

type
  TDeducModel = class
  private
    FVDed: Currency;
    FXDed: String;
  public
    property VDed: Currency read FVDed write FVDed;
    property XDed: String read FXDed write FXDed;
  end;

implementation

end.

