unit nfebr.request.cancela;

interface

uses
  nfebr.lib.enum;

type
  TNFeRequestCancela = class
  private
    FCNPJ: String;
    FChave: String;
    FOrgao: integer;
    FSequence: integer;
    FProtocolo: String;
    FJustificativa: String;
    FModelo: TDFeModelo;
    procedure _SetChave(const Value: String);
    procedure _SetJustificativa(const Value: String);
    procedure _SetModelo(const Value: TDFeModelo);
    procedure _SetProtocolo(const Value: String);
    procedure _SetCNPJ(const Value: String);
    procedure _SetSequence(const Value: integer);
    procedure _SetOrgao(const Value: integer);
  public
    property CNPJ: String read FCNPJ write _SetCNPJ;
    property Orgao: integer read FOrgao write _SetOrgao;
    property Chave: String read FChave write _SetChave;
    property Sequence: integer read FSequence write _SetSequence;
    property Protocolo: String read FProtocolo write _SetProtocolo;
    property Justificativa: String read FJustificativa write _SetJustificativa;
    property Modelo: TDFeModelo read FModelo write _SetModelo;
  end;

implementation

{ TNFeServicoCancelamento }

procedure TNFeRequestCancela._SetChave(const Value: String);
begin
  FChave := Value;
end;

procedure TNFeRequestCancela._SetCNPJ(const Value: String);
begin
  FCNPJ := Value;
end;

procedure TNFeRequestCancela._SetJustificativa(const Value: String);
begin
  FJustificativa := Value;
end;

procedure TNFeRequestCancela._SetModelo(const Value: TDFeModelo);
begin
  FModelo := Value;
end;

procedure TNFeRequestCancela._SetOrgao(const Value: integer);
begin
  FOrgao := Value;
end;

procedure TNFeRequestCancela._SetProtocolo(const Value: String);
begin
  FProtocolo := Value;
end;

procedure TNFeRequestCancela._SetSequence(const Value: integer);
begin
  FSequence := Value;
end;

end.

