unit nfebr.request.cancela.consulta;

interface

uses
  nfebr.lib.enum;

type
  TNFeRequestCancelaConsulta = class
  private
    FChave: String;
    FModelo: TDFeModelo;
    procedure _SetChave(const Value: String);
    procedure _SetModelo(const Value: TDFeModelo);
  public
    property Chave: String read FChave write _SetChave;
    property Modelo: TDFeModelo read FModelo write _SetModelo;
  end;

implementation

{ TNFeServicoConsulta }

procedure TNFeRequestCancelaConsulta._SetChave(const Value: String);
begin
  FChave := Value;
end;

procedure TNFeRequestCancelaConsulta._SetModelo(const Value: TDFeModelo);
begin
  FModelo := Value;
end;

end.

