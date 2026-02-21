unit nfebr.model;

interface

uses
  Generics.Collections,
  nfebr.tag.infnfe.model,
  nfebr.tag.infnfesupl.model;

type
  TNFeModel = class
  private
    FInfNFe: TInfNFeModel;
    FInfNFeSupl: TInfNFeSuplModel;
    FAmbiente: String;
    FReferencia: String;
  public
    constructor Create;
    destructor Destroy; override;
    property InfNFe: TInfNFeModel read FInfNFe write FInfNFe;
    property InfNFeSupl: TInfNFeSuplModel read FInfNFeSupl write FInfNFeSupl;
    property Ambiente: String read FAmbiente write FAmbiente;
    property Referencia: String read FReferencia write FReferencia;
  end;

implementation

{ TNFeModel }

constructor TNFeModel.Create;
begin
  FInfNFe := TInfNFeModel.Create;
  FInfNFeSupl := TInfNFeSuplModel.Create;
end;

destructor TNFeModel.Destroy;
begin
  FInfNFe.Free;
  FInfNFeSupl.Free;
  inherited;
end;

end.
