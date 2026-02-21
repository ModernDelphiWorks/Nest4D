unit nfebr.response.consulta;

interface

uses
  Generics.Collections,
  nfebr.response.base;

type
  TNFeResponseConsulta = class(TNFeResponseBase)
  private
    FChaveAcesso: String;
    FCodMsg: Integer;
    FMsg: String;
    FProtocolo: String;
    FDigValor: String;
  public
    property ChaveAcesso: String read FChaveAcesso write FChaveAcesso;
    property CodMsg: Integer read FCodMsg write FCodMsg;
    property Protocolo: String read FProtocolo write FProtocolo;
    property DigValor: String read FDigValor write FDigValor;
    property Msg: String read FMsg write FMsg;
  end;

implementation

end.
