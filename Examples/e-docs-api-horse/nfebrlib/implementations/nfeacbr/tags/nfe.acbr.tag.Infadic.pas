unit nfe.acbr.tag.Infadic;

interface

uses
  SysUtils,
  ACBrNFe,
  ACBrDFeDANFeReport,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
  nfebr.lib.enum,
  nfebr.model,
  nfe.acbr.interfaces;

type
  TNFeTagInfAdic = class(TInterfacedObject, INFeExecute)
  private
    FNFe: TACBrNFe;
  public
    class function Factory(ADFe: TACBrNFe): INFeExecute;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

{ TDFeTagInfAdic }

constructor TNFeTagInfAdic.Create(ADFe: TACBrNFe);
begin
  FNFe := ADFe;
end;

destructor TNFeTagInfAdic.Destroy;
begin
  FNFe := nil;
  inherited;
end;

procedure TNFeTagInfAdic.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
//  LInfCpl: TStringBuilder;
  LNFe: TNFe;
begin
  LNFe := FNFe.NotasFiscais.Items[AIndex].NFe;
//  LInfCpl := TStringBuilder.Create;
//  try
//    /// <summary> Somente NFe (55) </summary>
//    if LNFe.Ide.modelo = 55 then
//    begin
//      if LNFe.Emit.CRT = crtSimplesNacional then
//      begin
//        LInfCpl.AppendLine('DOCUMENTO EMITIDO POR ME OU EPP OPTANTE PELO SIMPLES NACIONAL ');
//        LInfCpl.Append    ('NAO GERA DIREITO A CREDITO FISCAL DE IPI.');
//        //
//        if ADFeDataSet.FieldByName('DFE_ICMSCREDITOSN').AsFloat > 0 then
//        begin
//          LInfCpl.AppendLine('PERMITE O APROVEITAMENTO DO CREDITO DE ICMS NO VALOR DE R$ ');
//          LInfCpl.Append    (FormatFloat('#,###,##0.00 ', ADFeDataSet.FieldByName('DFE_ICMSCREDITOSN').AsFloat));
//          LInfCpl.Append    ('CORRESPONDENTE A ALIQUOTA DE ');
//          LInfCpl.Append    (FormatFloat('0.00% ', AEMPDataSet.FieldByName('EMP_ICMSALIQUOTACREDITOSN').AsFloat));
//          LInfCpl.Append    ('NOS TERMOS DO ART. 23 DA LEI COMPLEMENTAR No 123, DE 2006.');
//        end;
//      end
//      else
//      if LNFe.Emit.CRT = crtSimplesExcessoReceita then
//      begin
//        LInfCpl.AppendLine('DOCUMENTO EMITIDO POR ME OU EPP OPTANTE PELO SIMPLES NACIONAL ');
//        LInfCpl.Append    ('ESTABELECIMENTO IMPEDIDO DE RECOLHER O ICMS/ISS PELO SIMPLES NACIONAL, ');
//        LInfCpl.Append    ('NOS TERMOS DO § 1º DO ART. 20 DA LC 123/2006, NAO GERA DIREITO A CREDITO FISCAL DE IPI.');
//      end;
//    end;
//    LInfCpl.AppendLine(ADFeDataSet.FieldByName('DFE_OBSERVACAO').AsString);
//    LNFe.InfAdic.infCpl := LInfCpl.ToString;
//    // Tributos aproximados incidentes sobre a nota
//    FDFe.DANFE.ImprimeTributos := trbSeparadamente;
//    FDFe.DANFE.vTribFed := ADFeDataSet.FieldByName('DFE_TRIBVALORNACIONAL').AsFloat;
//    FDFe.DANFE.vTribEst := ADFeDataSet.FieldByName('DFE_TRIBVALORESTADUAL').AsFloat;
//    FDFe.DANFE.vTribMun := ADFeDataSet.FieldByName('DFE_TRIBVALORMUNICIPAL').AsFloat;
//    FDFe.DANFE.FonteTributos := 'IBPT'; //ACBrIBPTax1.Fonte;
//    FDFe.DANFE.ChaveTributos := '';     //ACBrIBPTax1.ChaveArquivo;
//  finally
//    LInfCpl.Free;
//  end;
end;

class function TNFeTagInfAdic.Factory(ADFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(ADFe);
end;

end.
