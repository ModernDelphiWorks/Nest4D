unit nfe.acbr.tag.dest;

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
  TNFeTagDest = class(TInterfacedObject, INFeExecute)
  private
    FACBrNFe: TACBrNFe;
  public
    class function Factory(ADFe: TACBrNFe): INFeExecute;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

uses
  System.StrUtils;

{ TDFeTagDest }

constructor TNFeTagDest.Create(ADFe: TACBrNFe);
begin
  FACBrNFe := ADFe;
end;

destructor TNFeTagDest.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

procedure TNFeTagDest.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LNFe: TNFe;

  procedure DefineModelo55;
  begin
    LNFe.Ide.indFinal := TpcnConsumidorFinal(ANFeModel.InfNFe.Dest.CNPJCPF <> '');
    LNFe.Dest.indIEDest := TpcnindIEDest(ANFeModel.InfNFe.Dest.indIEDest);
    case LNFe.Dest.indIEDest of
      inContribuinte:
        LNFe.Dest.IE := ANFeModel.InfNFe.Dest.IE;
      inIsento:
        ;
      inNaoContribuinte:
        LNFe.Dest.IE := '';
    end;
  end;

begin
  LNFe := FACBrNFe.NotasFiscais.Items[AIndex].nfe;
  // Modelo 55 (NFe)
  DefineModelo55;
  //
  LNFe.Dest.CNPJCPF := ANFeModel.InfNFe.Dest.CNPJCPF;
  LNFe.Dest.xNome := ANFeModel.InfNFe.Dest.xNome;
  LNFe.Dest.ISUF := ANFeModel.InfNFe.Dest.ISUF;
  // TAG de grupo de endereço do Destinatário da NF-e - <enderDest> - Ocorrência 1-1
  LNFe.Dest.enderDest.xLgr := ANFeModel.InfNFe.Dest.enderDest.xLgr;
  LNFe.Dest.enderDest.nro := ANFeModel.InfNFe.Dest.enderDest.nro;
  LNFe.Dest.enderDest.xCpl := ANFeModel.InfNFe.Dest.enderDest.xCpl;
  LNFe.Dest.enderDest.xBairro := ANFeModel.InfNFe.Dest.enderDest.xBairro;
  LNFe.Dest.enderDest.cMun := ANFeModel.InfNFe.Dest.enderDest.cMun;
  LNFe.Dest.enderDest.xMun := ANFeModel.InfNFe.Dest.enderDest.xMun;
  LNFe.Dest.enderDest.UF := ANFeModel.InfNFe.Dest.enderDest.UF;
  LNFe.Dest.enderDest.CEP := ANFeModel.InfNFe.Dest.enderDest.CEP;
  LNFe.Dest.enderDest.cPais := ANFeModel.InfNFe.Dest.enderDest.cPais;
  LNFe.Dest.enderDest.xPais := ANFeModel.InfNFe.Dest.enderDest.xPais;
  LNFe.Dest.enderDest.fone := ANFeModel.InfNFe.Dest.enderDest.fone;
  // Só verifica a UF Origem vs Destino se tiver os dados do destinatário.
  if Length(LNFe.Dest.CNPJCPF) = 0 then
    Exit;
  if LNFe.Emit.enderEmit.UF <> LNFe.Dest.enderDest.UF then
    LNFe.Ide.idDest := doInterestadual;
end;

class function TNFeTagDest.Factory(ADFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(ADFe);
end;

end.
