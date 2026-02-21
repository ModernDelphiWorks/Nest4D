unit nfebr.response.cancela;

interface

uses
  Generics.Collections,
  nfebr.response.base;

type
  TNFeResponseCancela = class(TNFeResponseBase)
  private
    FChaveAcesso: String;
    FProtocolo: String;
    FTipoEvento: String;
    FJustificativa: String;
    FSequencia: integer;
    FCNPJDest: String;
    FEmailDest: String;
  public
    property ChaveAcesso: String read FChaveAcesso write FChaveAcesso;
    property Protocolo: String read FProtocolo write FProtocolo;
    property TipoEvento: String read FTipoEvento write FTipoEvento;
    property Justificativa: String read FJustificativa write FJustificativa;
    property Sequencia: integer read FSequencia write FSequencia;
    property CNPJDest: String read FCNPJDest write FCNPJDest;
    property EmailDest: String read FEmailDest write FEmailDest;
  end;

implementation

end.
