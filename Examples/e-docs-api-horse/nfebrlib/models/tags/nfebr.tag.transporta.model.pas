unit nfebr.tag.transporta.model;

interface

type
  TTransportaModel = class
  private
    FCNPJ: String;
    FCPF: String;
    FIE: String;
    FUF: String;
    FXEnder: String;
    FXMun: String;
    FXNome: String;
  public
    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
    property IE: String read FIE write FIE;
    property UF: String read FUF write FUF;
    property XEnder: String read FXEnder write FXEnder;
    property XMun: String read FXMun write FXMun;
    property XNome: String read FXNome write FXNome;
  end;

implementation

end.

