unit nfebr.tag.issqntot.model;

interface

type
  TISSQNtotModel = class
  private
    FCRegTrib: integer;
    FDCompet: TDateTime;
    FVBC: Currency;
    FVCOFINS: Currency;
    FVDeducao: Currency;
    FVDescCond: Currency;
    FVDescIncond: Currency;
    FVISS: Currency;
    FVISSRet: Currency;
    FVOutro: Currency;
    FVPIS: Currency;
    FVServ: Currency;
  public
    property CRegTrib: integer read FCRegTrib write FCRegTrib;
    property DCompet: TDateTime read FDCompet write FDCompet;
    property VBC: Currency read FVBC write FVBC;
    property VCOFINS: Currency read FVCOFINS write FVCOFINS;
    property VDeducao: Currency read FVDeducao write FVDeducao;
    property VDescCond: Currency read FVDescCond write FVDescCond;
    property VDescIncond: Currency read FVDescIncond write FVDescIncond;
    property VISS: Currency read FVISS write FVISS;
    property VISSRet: Currency read FVISSRet write FVISSRet;
    property VOutro: Currency read FVOutro write FVOutro;
    property VPIS: Currency read FVPIS write FVPIS;
    property VServ: Currency read FVServ write FVServ;
  end;

implementation

end.

