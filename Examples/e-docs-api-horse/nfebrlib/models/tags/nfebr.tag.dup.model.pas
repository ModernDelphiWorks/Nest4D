unit nfebr.tag.dup.model;

interface

type
  TDupModel = class
  private
    FDVenc: TDateTime;
    FNDup: String;
    FVDup: Currency;
  public
    property DVenc: TDateTime read FDVenc write FDVenc;
    property NDup: String read FNDup write FNDup;
    property VDup: Currency read FVDup write FVDup;
  end;

implementation

end.

