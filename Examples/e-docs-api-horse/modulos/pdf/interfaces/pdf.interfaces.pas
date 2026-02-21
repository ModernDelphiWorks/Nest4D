unit pdf.interfaces;

interface

uses
  core.types;

type
  IPDF = interface
    ['{AF65AA10-A104-42E7-ABEA-63E9DB062109}']
    function NFePDF(const AChave: String): TPDFResponse;
    function NFeCancelamentoPDF(const AChave: String): TPDFResponse;
    function NFeInutilizacaoPDF(const AProtocolo: String): TPDFResponse;
    function NFeCartaCorrecaoPDF(const AChave: String): TPDFResponse;
  end;

implementation

end.
