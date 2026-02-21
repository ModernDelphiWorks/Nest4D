unit nfebr.tag.enderdest.model;

interface

type
  TEnderDestModel = class
  private
    FCEP: integer;
    FCMun: integer;
    FCPais: integer;
    FFone: String;
    FNro: String;
    FUF: String;
    FXBairro: String;
    FXCpl: String;
    FXLgr: String;
    FXMun: String;
    FXPais: String;
  public
    property CEP: integer read FCEP write FCEP;
    property CMun: integer read FCMun write FCMun;
    property CPais: integer read FCPais write FCPais;
    property Fone: String read FFone write FFone;
    property Nro: String read FNro write FNro;
    property UF: String read FUF write FUF;
    property XBairro: String read FXBairro write FXBairro;
    property XCpl: String read FXCpl write FXCpl;
    property XLgr: String read FXLgr write FXLgr;
    property XMun: String read FXMun write FXMun;
    property XPais: String read FXPais write FXPais;
  end;

implementation

end.

