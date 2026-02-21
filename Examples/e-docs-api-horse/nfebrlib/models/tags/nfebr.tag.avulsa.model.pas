unit nfebr.tag.avulsa.model;

interface

type
  TAvulsaModel = class
  private
    FCNPJ: String;
    FDEmi: TDateTime;
    FDPag: TDateTime;
    FFone: String;
    FMatr: String;
    FNDAR: String;
    FRepEmi: String;
    FUF: String;
    FVDAR: Currency;
    FXAgente: String;
    FXOrgao: String;
  public
    property CNPJ: String read FCNPJ write FCNPJ;
    property DEmi: TDateTime read FDEmi write FDEmi;
    property DPag: TDateTime read FDPag write FDPag;
    property Fone: String read FFone write FFone;
    property Matr: String read FMatr write FMatr;
    property NDAR: String read FNDAR write FNDAR;
    property RepEmi: String read FRepEmi write FRepEmi;
    property UF: String read FUF write FUF;
    property VDAR: Currency read FVDAR write FVDAR;
    property XAgente: String read FXAgente write FXAgente;
    property XOrgao: String read FXOrgao write FXOrgao;
  end;

implementation

end.

