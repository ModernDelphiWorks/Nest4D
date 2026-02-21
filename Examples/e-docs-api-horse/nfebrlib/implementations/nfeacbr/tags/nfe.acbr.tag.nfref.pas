unit nfe.acbr.tag.nfref;

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
  TNFeTagNFref = class(TInterfacedObject, INFeExecute)
  private
    FNFe: TACBrNFe;
  public
    class function Factory(ADFe: TACBrNFe): TNFeTagNFref;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

{ TDFeTagNFref }

constructor TNFeTagNFref.Create(ADFe: TACBrNFe);
begin
  FNFe := ADFe;
end;

destructor TNFeTagNFref.Destroy;
begin
  FNFe := nil;
  inherited;
end;

procedure TNFeTagNFref.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LNFref: TNFrefCollectionItem;
  LFor: integer;
begin
  for LFor := 0 to ANFeModel.InfNFe.Ide.NFref.Count - 1 do
  begin
    LNFref := FNFe.NotasFiscais.Items[AIndex].NFe.Ide.NFref.New;
    LNFref.refNFe := ANFeModel.InfNFe.Ide.NFref.Items[LFor].RefNFe;
  end;
end;

class function TNFeTagNFref.Factory(ADFe: TACBrNFe): TNFeTagNFref;
begin
  Result := Self.Create(ADFe);
end;

end.

