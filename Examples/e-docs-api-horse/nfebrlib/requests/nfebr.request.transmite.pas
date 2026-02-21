unit nfebr.request.transmite;

interface

uses
  NFebr.model,
  nfebr.lib.enum;

type
  TNFeRequestTransmite = class
  private
    FNFeModel: TNFeModel;
    FModelo: TDFeModelo;
    procedure _SetModelo(const Value: TDFeModelo);
    procedure _SetNFeModel(const Value: TNFeModel);
  public
    property NotaFiscal: TNFeModel read FNFeModel write _SetNFeModel;
    property Modelo: TDFeModelo read FModelo write _SetModelo;
  end;

implementation

{ TNFeServicoTransmite }

procedure TNFeRequestTransmite._SetModelo(const Value: TDFeModelo);
begin
  FModelo := Value;
end;

procedure TNFeRequestTransmite._SetNFeModel(const Value: TNFeModel);
begin
  FNFeModel := Value;
end;

end.

