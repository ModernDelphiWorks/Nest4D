unit nfebr.tag.refnf.model;

interface

type
  TRefNFModel = class
  private
    FAAMM: String;
    FCNPJ: String;
    FCUF: integer;
    FMod: String;
    FNNF: integer;
    FSerie: integer;
  public
    property AAMM: String read FAAMM write FAAMM;
    property CNPJ: String read FCNPJ write FCNPJ;
    property CUF: integer read FCUF write FCUF;
    property &Mod: String read FMod write FMod;
    property NNF: integer read FNNF write FNNF;
    property Serie: integer read FSerie write FSerie;
  end;

implementation

end.

