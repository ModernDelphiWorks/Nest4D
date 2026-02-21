unit nfebr.tag.veictransp.model;

interface

type
  TVeicTranspModel = class
  private
    FPlaca: String;
    FRNTC: String;
    FUF: String;
  public
    property Placa: String read FPlaca write FPlaca;
    property RNTC: String read FRNTC write FRNTC;
    property UF: String read FUF write FUF;
  end;

implementation

end.

