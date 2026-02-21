unit nfebr.tag.issqn.model;

interface

type
  TISSQNModel = class
  private
    FCListServ: String;
    FCMun: integer;
    FCMunFG: String;
    FCPais: String;
    FCServico: String;
    FIndISS: integer;
    FIndIncentivo: integer;
    FNProcesso: String;
    FVAliq: Currency;
    FVBC: Currency;
    FVDeducao: Currency;
    FVDescCond: Currency;
    FVDescIncond: Currency;
    FVISSQN: Currency;
    FVISSRet: Currency;
    FVOutro: Currency;
  public
    property CListServ: String read FCListServ write FCListServ;
    property CMun: integer read FCMun write FCMun;
    property CMunFG: String read FCMunFG write FCMunFG;
    property CPais: String read FCPais write FCPais;
    property CServico: String read FCServico write FCServico;
    property IndISS: integer read FIndISS write FIndISS;
    property IndIncentivo: integer read FIndIncentivo write FIndIncentivo;
    property NProcesso: String read FNProcesso write FNProcesso;
    property VAliq: Currency read FVAliq write FVAliq;
    property VBC: Currency read FVBC write FVBC;
    property VDeducao: Currency read FVDeducao write FVDeducao;
    property VDescCond: Currency read FVDescCond write FVDescCond;
    property VDescIncond: Currency read FVDescIncond write FVDescIncond;
    property VISSQN: Currency read FVISSQN write FVISSQN;
    property VISSRet: Currency read FVISSRet write FVISSRet;
    property VOutro: Currency read FVOutro write FVOutro;
  end;

implementation

end.

