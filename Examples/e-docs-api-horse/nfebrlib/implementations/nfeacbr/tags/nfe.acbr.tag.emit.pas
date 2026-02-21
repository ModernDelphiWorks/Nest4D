unit nfe.acbr.tag.emit;

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
  TNFeTagEmit = class(TInterfacedObject, INFeExecute)
  private
    FNFe: TACBrNFe;
  public
    class function Factory(ADFe: TACBrNFe): TNFeTagEmit;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

uses
  nfebr.tag.emit.model;

{ TDFeTagEmit }

constructor TNFeTagEmit.Create(ADFe: TACBrNFe);
begin
  FNFe := ADFe;
end;

destructor TNFeTagEmit.Destroy;
begin
  FNFe := nil;
  inherited;
end;

procedure TNFeTagEmit.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LNFeEmitTag: TEmit;
  LEmitModel: TEmitModel;
begin
  LNFeEmitTag := FNFe.NotasFiscais.Items[AIndex].nfe.emit;
  LEmitModel := ANFeModel.InfNFe.emit;
  //
  LNFeEmitTag.CNPJCPF := LEmitModel.CNPJCPF;
  LNFeEmitTag.xNome := LEmitModel.xNome;
  LNFeEmitTag.xFant := LEmitModel.xFant;
  LNFeEmitTag.IE := LEmitModel.IE;
  LNFeEmitTag.IEST := LEmitModel.IEST;
  LNFeEmitTag.IM := LEmitModel.IM;
  LNFeEmitTag.CNAE := LEmitModel.CNAE;
  // TAG de grupo do Endereço do emitente - <enderEmit> - Ocorrência 1-1
  LNFeEmitTag.enderEmit.xLgr := LEmitModel.enderEmit.xLgr;
  LNFeEmitTag.enderEmit.nro := LEmitModel.enderEmit.nro;
  LNFeEmitTag.enderEmit.xCpl := LEmitModel.enderEmit.xCpl;
  LNFeEmitTag.enderEmit.xBairro := LEmitModel.enderEmit.xBairro;
  LNFeEmitTag.enderEmit.cMun := StrToIntDef(LEmitModel.enderEmit.cMun, 0);
  LNFeEmitTag.enderEmit.xMun := LEmitModel.enderEmit.xMun;
  LNFeEmitTag.enderEmit.UF := LEmitModel.enderEmit.UF;
  LNFeEmitTag.enderEmit.CEP := StrToIntDef(LEmitModel.enderEmit.CEP, 0);
  LNFeEmitTag.enderEmit.cPais := StrToIntDef(LEmitModel.enderEmit.cPais, 1058);
  LNFeEmitTag.enderEmit.xPais := LEmitModel.enderEmit.xPais;
  LNFeEmitTag.enderEmit.fone := LEmitModel.enderEmit.fone;
  // 0 - crtSimplesNacional,
  // 1 - crtSimplesExcessoReceita,
  // 2 - crtRegimeNormal
  LNFeEmitTag.CRT := TpcnCRT(LEmitModel.CRT);
end;

class function TNFeTagEmit.Factory(ADFe: TACBrNFe): TNFeTagEmit;
begin
  Result := Self.Create(ADFe);
end;

end.
