unit nfebr.tag.autxml.model;

interface

type
  TAutXMLModel = class
  private
    FCNPJ: String;
    FCPF: String;
  public
    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
  end;

implementation

end.

