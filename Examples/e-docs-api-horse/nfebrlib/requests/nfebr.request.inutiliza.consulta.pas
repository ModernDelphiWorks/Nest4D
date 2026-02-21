unit nfebr.request.inutiliza.consulta;

interface

uses
  nfebr.lib.enum;

type
  TNFeRequestInutilizaConsulta = class
  private
    FSerie: String;
    FNumeroInicial: integer;
    FNumeroFinal: integer;
    FModelo: TDFeModelo;
    procedure _SetChave(const Value: String);
    procedure _SetModelo(const Value: TDFeModelo);
    procedure _SetNumeroFinal(const Value: integer);
    procedure _SetNumeroInicial(const Value: integer);
  public
    property Serie: String read FSerie write _SetChave;
    property NumeroInicial: integer read FNumeroInicial write _SetNumeroInicial;
    property NumeroFinal: integer read FNumeroFinal write _SetNumeroFinal;
    property Modelo: TDFeModelo read FModelo write _SetModelo;
  end;

implementation

{ TNFeServicoConsulta }

procedure TNFeRequestInutilizaConsulta._SetChave(const Value: String);
begin
  FSerie := Value;
end;

procedure TNFeRequestInutilizaConsulta._SetModelo(const Value: TDFeModelo);
begin
  FModelo := Value;
end;

procedure TNFeRequestInutilizaConsulta._SetNumeroFinal(const Value: integer);
begin
  FNumeroFinal := Value;
end;

procedure TNFeRequestInutilizaConsulta._SetNumeroInicial(const Value: integer);
begin
  FNumeroInicial := Value;
end;

end.
