unit nfebr.tag.refnfp.model;

interface

type
  TRefNFPModel = class
  private
    FAAMM: String;
    FCNPJ: String;
    FCPF: String;
    FCUF: integer;
    FIE: String;
    FMod: String;
    FNNF: integer;
    FSerie: integer;
  public
    property AAMM: String read FAAMM write FAAMM;
    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
    property CUF: integer read FCUF write FCUF;
    property IE: String read FIE write FIE;
    property &Mod: String read FMod write FMod;
    property NNF: integer read FNNF write FNNF;
    property Serie: integer read FSerie write FSerie;
  end;

implementation

end.

