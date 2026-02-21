unit nfebr.tag.infprodnff.model;

interface

type
  TInfProdNFFModel = class
  private
    FCOperNFF: String;
    FCProdFisco: String;
  public
    property COperNFF: String read FCOperNFF write FCOperNFF;
    property CProdFisco: String read FCProdFisco write FCProdFisco;
  end;

implementation

end.

