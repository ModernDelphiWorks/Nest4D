unit nfebr.tag.encerrante.model;

interface

type
  TEncerranteModel = class
  private
    FNBico: integer;
    FNBomba: integer;
    FNTanque: integer;
    FVEncFin: Currency;
    FVEncIni: Currency;
  public
    property NBico: integer read FNBico write FNBico;
    property NBomba: integer read FNBomba write FNBomba;
    property NTanque: integer read FNTanque write FNTanque;
    property VEncFin: Currency read FVEncFin write FVEncFin;
    property VEncIni: Currency read FVEncIni write FVEncIni;
  end;

implementation

end.

