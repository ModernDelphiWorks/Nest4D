unit nfebr.tag.fordia.model;

interface

type
  TForDiaModel = class
  private
    FDia: integer;
    FQtde: Double;
  public
    property Dia: integer read FDia write FDia;
    property Qtde: Double read FQtde write FQtde;
  end;

implementation

end.

