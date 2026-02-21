unit nfebr.response.servicostatus;

interface

uses
  Generics.Collections,
  nfebr.response.base;

type
  TNFeResponseServicoStatus = class(TNFeResponseBase)
  private
    FDataRetorno: TDateTime;
    FTMResposta: integer;
    FObs: String;
  public
    property DataRetorno: TDateTime read FDataRetorno write FDataRetorno;
    property TMResposta: integer read FTMResposta write FTMResposta;
    property Obs: String read FObs write FObs;
  end;

implementation

end.
