unit nfe.acbr.tag.transpvol;

interface

uses
  SysUtils,
  ACBrNFe,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
  nfebr.lib.enum,
  nfebr.model,
  nfe.acbr.interfaces;

type
  TNFeTagTransVol = class(TInterfacedObject, INFeExecute)
  private
    FNFe: TACBrNFe;
  public
    class function Factory(ADFe: TACBrNFe): INFeExecute;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

{ TDFeTagTransVol }

constructor TNFeTagTransVol.Create(ADFe: TACBrNFe);
begin
  FNFe := ADFe;
end;

destructor TNFeTagTransVol.Destroy;
begin
  FNFe := nil;
  inherited;
end;

procedure TNFeTagTransVol.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LTransp: TTransp;
  LVol: TVolCollectionItem;
  LFor: integer;
begin
  LTransp := FNFe.NotasFiscais.Items[AIndex].NFe.Transp;
  LTransp.Vol.Clear;
  for LFor := 0 to ANFeModel.InfNFe.Transp.Vol.Count -1 do
  begin
    LVol := LTransp.Vol.New;
    LVol.esp := ANFeModel.InfNFe.Transp.Vol.Items[LFor].Esp;
    LVol.marca := ANFeModel.InfNFe.Transp.Vol.Items[LFor].Marca;
    LVol.nVol := ANFeModel.InfNFe.Transp.Vol.Items[LFor].NVol;
    LVol.qVol := ANFeModel.InfNFe.Transp.Vol.Items[LFor].QVol;
    LVol.pesoB := ANFeModel.InfNFe.Transp.Vol.Items[LFor].PesoB;
    LVol.pesoL := ANFeModel.InfNFe.Transp.Vol.Items[LFor].PesoL;
  end;
end;

class function TNFeTagTransVol.Factory(ADFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(ADFe);
end;

end.
