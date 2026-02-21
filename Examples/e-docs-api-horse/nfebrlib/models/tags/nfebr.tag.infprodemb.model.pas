unit nfebr.tag.infprodemb.model;

interface

type
  TInfProdEmbModel = class
  private
    FQVolEmb: Double;
    FUEmb: String;
    FXEmb: String;
  public
    property QVolEmb: Double read FQVolEmb write FQVolEmb;
    property UEmb: String read FUEmb write FUEmb;
    property XEmb: String read FXEmb write FXEmb;
  end;

implementation

end.

