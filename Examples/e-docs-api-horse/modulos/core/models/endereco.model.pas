unit endereco.model;

interface

type
  TEnderecoModel = class
  private
    FBairro: String;
    FCep: String;
    FCidade: String;
    FCodigoMunicipio: String;
    FCodigoPais: String;
    FComplemento: String;
    FLogradouro: String;
    FNumero: String;
    FPais: String;
    FUf: String;
  public
    property Bairro: String read FBairro write FBairro;
    property Cep: String read FCep write FCep;
    property Cidade: String read FCidade write FCidade;
    property CodigoMunicipio: String read FCodigoMunicipio write FCodigoMunicipio;
    property CodigoPais: String read FCodigoPais write FCodigoPais;
    property Complemento: String read FComplemento write FComplemento;
    property Logradouro: String read FLogradouro write FLogradouro;
    property Numero: String read FNumero write FNumero;
    property Pais: String read FPais write FPais;
    property Uf: String read FUf write FUf;
  end;

implementation

end.
