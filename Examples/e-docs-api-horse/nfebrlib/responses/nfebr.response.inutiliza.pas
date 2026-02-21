unit nfebr.response.inutiliza;

interface

uses
  Generics.Collections,
  nfebr.response.base;

type
  TNFeResponseInutiliza = class(TNFeResponseBase)
  private
    FProtocolo: String;
    FCnpj: String;
    FJustificativa: String;
    FAno: integer;
    FSerie: integer;
    FNumInicial: integer;
    FNumFinal: integer;
    FModelo: integer;
  public
    property Protocolo: String read FProtocolo write FProtocolo;
    property Cnpj: String read FCnpj write FCnpj;
    property Justificativa: String read FJustificativa write FJustificativa;
    property Ano: integer read FAno write FAno;
    property Modelo: integer read FModelo write FModelo;
    property NumFinal: integer read FNumFinal write FNumFinal;
    property NumInicial: integer read FNumInicial write FNumInicial;
    property Serie: integer read FSerie write FSerie;
  end;

implementation

end.
