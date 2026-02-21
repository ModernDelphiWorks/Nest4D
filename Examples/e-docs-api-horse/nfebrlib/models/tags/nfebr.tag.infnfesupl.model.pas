unit nfebr.tag.infnfesupl.model;

interface

type
  TInfNFeSuplModel = class
  private
    FQrCode: String;
    FUrlChave: String;
  public
    property QrCode: String read FQrCode write FQrCode;
    property UrlChave: String read FUrlChave write FUrlChave;
  end;

implementation

end.
