unit nfebr.request.consulta;

interface

uses
  nfebr.lib.enum;

type
  TNFeRequestConsulta = class
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

procedure TNFeRequestConsulta._SetChave(const Value: String);
begin
  FChave := Value;
end;

procedure TNFeRequestConsulta._SetModelo(const Value: TDFeModelo);
begin
  FModelo := Value;
end;

end.

