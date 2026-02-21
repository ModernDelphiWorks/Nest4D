unit nfebr.tag.refecf.model;

interface

type
  TRefECFModel = class
  private
    FMod: String;
    FNCOO: integer;
    FNECF: integer;
  public
    property &Mod: String read FMod write FMod;
    property NCOO: integer read FNCOO write FNCOO;
    property NECF: integer read FNECF write FNECF;
  end;

implementation

end.

