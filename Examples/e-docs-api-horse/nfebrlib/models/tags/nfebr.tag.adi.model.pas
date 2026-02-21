unit nfebr.tag.adi.model;

interface

type
  TAdiModel = class
  private
    FCFabricante: String;
    FNAdicao: integer;
    FNDraw: String;
    FNSeqAdic: integer;
    FVDescDI: Currency;
  public
    property CFabricante: String read FCFabricante write FCFabricante;
    property NAdicao: integer read FNAdicao write FNAdicao;
    property NDraw: String read FNDraw write FNDraw;
    property NSeqAdic: integer read FNSeqAdic write FNSeqAdic;
    property VDescDI: Currency read FVDescDI write FVDescDI;
  end;

implementation

end.

