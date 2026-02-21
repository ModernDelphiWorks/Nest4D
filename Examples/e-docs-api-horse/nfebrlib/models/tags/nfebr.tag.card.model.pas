unit nfebr.tag.card.model;

interface

type
  TCardModel = class
  private
    FCAut: String;
    FCNPJ: String;
    FTBand: String;
    FTpIntegra: integer;
  public
    property CAut: String read FCAut write FCAut;
    property CNPJ: String read FCNPJ write FCNPJ;
    property TBand: String read FTBand write FTBand;
    property TpIntegra: integer read FTpIntegra write FTpIntegra;
  end;

implementation

end.

