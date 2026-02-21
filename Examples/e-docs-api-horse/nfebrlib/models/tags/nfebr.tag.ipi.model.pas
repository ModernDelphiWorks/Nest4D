unit nfebr.tag.ipi.model;

interface

type
  TIPIModel = class
  private
    FCST: String;
    FCEnq: String;
    FCLEnq: String;
    FCNPJProd: String;
    FCSelo: String;
    FQSelo: integer;
    FPIPI: Double;
    FQUnId: Double;
    FVBC: Currency;
    FVIPI: Currency;
    FVUnId: Currency;
    FVIPIDevol: Currency;
  public
    property CST: String read FCST write FCST;
    property CEnq: String read FCEnq write FCEnq;
    property CLEnq: String read FCLEnq write FCLEnq;
    property CNPJProd: String read FCNPJProd write FCNPJProd;
    property CSelo: String read FCSelo write FCSelo;
    property QSelo: integer read FQSelo write FQSelo;
    property VIPIDevol: Currency read FVIPIDevol write FVIPIDevol;
    property PIPI: Double read FPIPI write FPIPI;
    property QUnId: Double read FQUnId write FQUnId;
    property VBC: Currency read FVBC write FVBC;
    property VIPI: Currency read FVIPI write FVIPI;
    property VUnId: Currency read FVUnId write FVUnId;
  end;

implementation

end.
