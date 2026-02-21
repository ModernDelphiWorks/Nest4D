unit nfebr.tag.retirada.model;

interface

type
  TRetiradaModel = class
  private
    FCEP: String;
    FCMun: integer;
    FCNPJ: String;
    FCPF: String;
    FCPais: String;
    FEmail: String;
    FFone: String;
    FIE: String;
    FNro: String;
    FUF: String;
    FXBairro: String;
    FXCpl: String;
    FXLgr: String;
    FXMun: String;
    FXNome: String;
    FXPais: String;
  public
    property CEP: String read FCEP write FCEP;
    property CMun: integer read FCMun write FCMun;
    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
    property CPais: String read FCPais write FCPais;
    property Email: String read FEmail write FEmail;
    property Fone: String read FFone write FFone;
    property IE: String read FIE write FIE;
    property Nro: String read FNro write FNro;
    property UF: String read FUF write FUF;
    property XBairro: String read FXBairro write FXBairro;
    property XCpl: String read FXCpl write FXCpl;
    property XLgr: String read FXLgr write FXLgr;
    property XMun: String read FXMun write FXMun;
    property XNome: String read FXNome write FXNome;
    property XPais: String read FXPais write FXPais;
  end;

implementation

end.

