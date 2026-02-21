unit nfebr.tag.arma.model;

interface

type
  TArmaModel = class
  private
    FDescr: String;
    FNCano: String;
    FNSerie: String;
    FTpArma: integer;
  public
    property Descr: String read FDescr write FDescr;
    property NCano: String read FNCano write FNCano;
    property NSerie: String read FNSerie write FNSerie;
    property TpArma: integer read FTpArma write FTpArma;
  end;

implementation

end.

