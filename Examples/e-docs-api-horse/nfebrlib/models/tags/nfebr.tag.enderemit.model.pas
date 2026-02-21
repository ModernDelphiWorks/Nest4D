unit nfebr.tag.enderemit.model;

interface

type
  TEnderEmitModel = class
  private
    FCEP: String;
    FCMun: String;
    FCPais: String;
    FFone: String;
    FNro: String;
    FUF: String;
    FXBairro: String;
    FXCpl: String;
    FXLgr: String;
    FXMun: String;
    FXPais: String;
  public
    property CEP: String read FCEP write FCEP;
    property CMun: String read FCMun write FCMun;
    property CPais: String read FCPais write FCPais;
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

