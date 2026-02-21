unit xml.interfaces;

interface

uses
  core.types;

type
  IXML = interface
    ['{37A57689-BC86-4C6C-B09A-ABE2F26D98BF}']
    function NFeXML(const AChave: String): TXMLResponse;
    function NFeCancelamentoXML(const AChave: String): TXMLResponse;
    function NFeInutilizacaoXML(const AProtocolo: String): TXMLResponse;
    function NFeCartaCorrecaoXML(const AChave: String): TXMLResponse;
  end;

implementation

end.
