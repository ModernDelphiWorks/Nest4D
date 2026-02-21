unit nfebr.request.inutiliza;

interface

uses
  nfebr.lib.enum;

type
  TNFeRequestInutiliza = class
  private
    FNumeroInicial: integer;
    FNumeroFinal: integer;
    FAno: integer;
    FSerie: integer;
    FJustificativa: String;
    FModelo: TDFeModelo;
    procedure _SetAno(const Value: integer);
    procedure _SetJustificativa(const Value: String);
    procedure _SetModelo(const Value: TDFeModelo);
    procedure _SetNumeroFinal(const Value: integer);
    procedure _SetNumeroInicial(const Value: integer);
    procedure _SetSerie(const Value: integer);
  public
    property NumeroInicial: integer read FNumeroInicial write _SetNumeroInicial;
    property NumeroFinal: integer read FNumeroFinal write _SetNumeroFinal;
    property Ano: integer read FAno write _SetAno;
    property Serie: integer read FSerie write _SetSerie;
    property Justificativa: String read FJustificativa write _SetJustificativa;
    property Modelo: TDFeModelo read FModelo write _SetModelo;
  end;

implementation

{ TNFeServicoInutiliza }

procedure TNFeRequestInutiliza._SetAno(const Value: integer);
begin
  FAno := Value;
end;

procedure TNFeRequestInutiliza._SetJustificativa(const Value: String);
begin
  FJustificativa := Value;
end;

procedure TNFeRequestInutiliza._SetModelo(const Value: TDFeModelo);
begin
  FModelo := Value;
end;

procedure TNFeRequestInutiliza._SetNumeroFinal(const Value: integer);
begin
  FNumeroFinal := Value;
end;

procedure TNFeRequestInutiliza._SetNumeroInicial(const Value: integer);
begin
  FNumeroInicial := Value;
end;

procedure TNFeRequestInutiliza._SetSerie(const Value: integer);
begin
  FSerie := Value;
end;

end.

