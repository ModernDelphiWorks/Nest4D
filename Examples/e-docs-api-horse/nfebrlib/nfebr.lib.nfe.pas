unit nfebr.lib.nfe;

interface

uses
  SysUtils,
  Generics.Collections,
  nfebr.lib.enum,
  nfebr.lib.include,
  nfebr.lib.utils,
  nfebr.model;

type
  INFeLib = interface
    ['{A00FA66F-A33B-4F64-99B1-2A8FDBAF9C22}']
    function Cancelar(const AReq: TNFeRequestCancela): TNFeResponseCancela;
    function CartaCorrecao(const AReq: TNFeRequestCCe): TNFeResponseCCe;
    function Consultar(const AReq: TNFeRequestConsulta): TNFeResponseConsulta;
    function Inutilizar(const AReq: TNFeRequestInutiliza): TNFeResponseInutiliza;
    function Transmitir(const AReq: TNFeRequestTransmite): TNFeResponseTransmite;
    function TransmitirLote(const AReq: TObjectList<TNFeRequestTransmite>): TNFeResponseTransmiteLote;
    function ServicoStatus: TNFeResponseServicoStatus;
    procedure SetConfigracao(const AConfig: TNFeConfig);
    procedure SetListener(const AListener: TProc<TObject, String>);
    procedure SetLogger(const ALog: TProc<String>);
    procedure GerarPDF;
  end;

implementation

end.


