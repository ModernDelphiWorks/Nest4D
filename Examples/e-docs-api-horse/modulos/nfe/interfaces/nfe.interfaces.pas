unit nfe.interfaces;

interface

uses
  core.include,
  core.types;

type
  INFe = interface
    ['{5F203028-7125-43CD-9AE3-0FFE7DC98080}']
    function NFeTransmitir(const AJson: String): TNFeResponse;
    function NFeTransmitirLote(const AJson: String): TNFeResponse;
    function NFeConsultar(const AJson: String): TNFeResponse;
    function NFeCancelar(const AJson: String): TNFeResponse;
    function NFeInutilizar(const AJson: String): TNFeResponse;
    function NFeCorrigir(const AJson: String): TNFeResponse;
    function NFeServicoStatus: TNFeResponse;
  end;

implementation

end.
