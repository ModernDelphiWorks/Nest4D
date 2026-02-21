unit nfebr.response.base;

interface

type
  TNFeResponseBase = class
  private
    FVersao: String;
    FAmbiente: String;
    FVerAplic: String;
    FCodStatus: integer;
    FMotivo: String;
    FUF: integer;
    FdhRecbto: TDateTime;
    FMensagem: String;
  public
    property Versao: String read Fversao write Fversao;
    property Ambiente: String read FAmbiente write FAmbiente;
    property VerAplic: String read FVerAplic write FVerAplic;
    property CodStatus: integer read FCodStatus write FCodStatus;
    property Motivo: String read FMotivo write FMotivo;
    property UF: integer read FUF write FUF;
    property DataRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property Mensagem: String read FMensagem write FMensagem;
  end;

implementation

end.
