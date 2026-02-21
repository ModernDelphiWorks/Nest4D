unit nfe.acbr.tag.resptec;

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
  TNFeTagRespTec = class(TInterfacedObject, INFeExecute)
  private
    FACBrNFe: TACBrNFe;
  public
    class function Factory(AACBrNFe: TACBrNFe): INFeExecute;
    constructor Create(AACBrNFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

{ TDFeTagRespTec }

constructor TNFeTagRespTec.Create(AACBrNFe: TACBrNFe);
begin
  FACBrNFe := AACBrNFe;
end;

destructor TNFeTagRespTec.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

procedure TNFeTagRespTec.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LNFe: TNFe;
begin
  LNFe := FACBrNFe.NotasFiscais.Items[AIndex].NFe;
  LNFe.infRespTec.CNPJ := ANFeModel.InfNFe.InfRespTec.CNPJ;
  LNFe.infRespTec.xContato := ANFeModel.InfNFe.InfRespTec.XContato;
  LNFe.infRespTec.email := ANFeModel.InfNFe.InfRespTec.Email;
  LNFe.infRespTec.fone := ANFeModel.InfNFe.InfRespTec.Fone;
  LNFe.infRespTec.idCSRT := ANFeModel.InfNFe.InfRespTec.IdCSRT;
  LNFe.infRespTec.hashCSRT := ANFeModel.InfNFe.InfRespTec.HashCSRT;
end;

class function TNFeTagRespTec.Factory(AACBrNFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(AACBrNFe);
end;

end.
