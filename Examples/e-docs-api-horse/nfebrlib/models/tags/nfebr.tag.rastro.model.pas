unit nfebr.tag.rastro.model;

interface

type
  TRastroModel = class
  private
    FCAgreg: String;
    FDFab: TDateTime;
    FDVal: TDateTime;
    FNLote: String;
    FQLote: Double;
  public
    property CAgreg: String read FCAgreg write FCAgreg;
    property DFab: TDateTime read FDFab write FDFab;
    property DVal: TDateTime read FDVal write FDVal;
    property NLote: String read FNLote write FNLote;
    property QLote: Double read FQLote write FQLote;
  end;

implementation

end.

