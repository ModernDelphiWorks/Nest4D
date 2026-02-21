unit nfebr.request.cce.consulta;

interface

uses
  nfebr.lib.enum;

type
  TNFeRequestCCeConsulta = class
  private
    FChave: String;
    FCorrecao: String;
    FModelo: TDFeModelo;
    procedure _SetChave(const Value: String);
    procedure _SetModelo(const Value: TDFeModelo);
    procedure _SetCorrecao(const Value: String);
  public
    property Chave: String read FChave write _SetChave;
    property Correcao: String read FCorrecao write _SetCorrecao;
    property Modelo: TDFeModelo read FModelo write _SetModelo;
  end;

implementation

{ TNFeServicoConsulta }

procedure TNFeRequestCCeConsulta._SetChave(const Value: String);
begin
  FChave := Value;
end;

procedure TNFeRequestCCeConsulta._SetCorrecao(const Value: String);
begin
  FCorrecao := Value;
end;

procedure TNFeRequestCCeConsulta._SetModelo(const Value: TDFeModelo);
begin
  FModelo := Value;
end;

end.

