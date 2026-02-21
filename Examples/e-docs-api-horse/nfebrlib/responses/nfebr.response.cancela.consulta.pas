unit nfebr.response.cancela.consulta;

interface

uses
  Generics.Collections,
  nfebr.response.base;

type
  TNFeResponseInfoCancelaConsulta = class;

  TNFeResponseCancelaConsulta = class(TNFeResponseBase)
    FChaveAcesso: String;
    FCodMsg: Integer;
    FMsg: String;
    FProtocolo: String;
    FDigValor: String;
    FInfoCancela: TNFeResponseInfoCancelaConsulta;
  public
    constructor Create;
    destructor Destroy; override;
    property ChaveAcesso: String read FChaveAcesso write FChaveAcesso;
    property CodMsg: Integer read FCodMsg write FCodMsg;
    property Protocolo: String read FProtocolo write FProtocolo;
    property DigValor: String read FDigValor write FDigValor;
    property Msg: String read FMsg write FMsg;
    property InfoCancela: TNFeResponseInfoCancelaConsulta read FInfoCancela write FInfoCancela;
  end;

  TNFeResponseInfoCancelaConsulta = class(TNFeResponseBase)
    FChaveAcesso: String;
    FProtocolo: String;
  public
    property ChaveAcesso: String read FChaveAcesso write FChaveAcesso;
    property Protocolo: String read FProtocolo write FProtocolo;
  end;


implementation

{ TNFeResponseCancelaConsulta }

constructor TNFeResponseCancelaConsulta.Create;
begin
  FInfoCancela := TNFeResponseInfoCancelaConsulta.Create;
end;

destructor TNFeResponseCancelaConsulta.Destroy;
begin
  FInfoCancela.Free;
  inherited;
end;

end.
