unit nfebr.request.cce;

interface

uses
  nfebr.lib.enum;

type
  TNFeRequestCCe = class
    FCNPJ: String;
    FChave: String;
    FOrgao: integer;
    FSequence: integer;
    FProtocolo: String;
    FCorrecao: String;
    FModelo: TDFeModelo;
    procedure _SetChave(const Value: String);
    procedure _SetCorrecao(const Value: String);
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
    property Correcao: String read FCorrecao write _SetCorrecao;
    property Modelo: TDFeModelo read FModelo write _SetModelo;
  end;

implementation

{ TNFeServicoCartaCorrecao }

procedure TNFeRequestCCe._SetCNPJ(const Value: String);
begin
  FCNPJ := Value;
end;

procedure TNFeRequestCCe._SetCorrecao(const Value: String);
begin
  FCorrecao := Value;
end;

procedure TNFeRequestCCe._SetChave(const Value: String);
begin
  FChave := Value;
end;

procedure TNFeRequestCCe._SetModelo(const Value: TDFeModelo);
begin
  FModelo := Value;
end;

procedure TNFeRequestCCe._SetOrgao(const Value: integer);
begin
  FOrgao := Value;
end;

procedure TNFeRequestCCe._SetProtocolo(const Value: String);
begin
  FProtocolo := Value;
end;

procedure TNFeRequestCCe._SetSequence(const Value: integer);
begin
  FSequence := Value;
end;

end.

