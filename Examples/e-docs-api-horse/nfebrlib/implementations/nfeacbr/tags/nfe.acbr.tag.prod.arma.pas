unit nfe.acbr.tag.prod.arma;

interface

uses
  SysUtils,
  ACBrNFe,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
  nfebr.lib.enum,
  nfebr.model,
  nfe.acbr.interfaces,
  nfebr.tag.det.model;

type
  TNFeTagProdArma = class(TInterfacedObject, INFeImposto)
  private
    FDFe: TACBrNFe;
  public
    class function New(ADFe: TACBrNFe): INFeImposto;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const ADetItem: TDetCollectionItem;
      const ANFeModel: TNFeModel; const ADetModel: TDetModel);
  end;

implementation

{ TDFeTagProdArma }

constructor TNFeTagProdArma.Create(ADFe: TACBrNFe);
begin
  FDFe := ADFe;
end;

destructor TNFeTagProdArma.Destroy;
begin

  inherited;
end;

procedure TNFeTagProdArma.Execute(const ADetItem: TDetCollectionItem;
  const ANFeModel: TNFeModel; const ADetModel: TDetModel);
begin
////   try
////     if not rR01_4.Eof then
////     begin
////        AProd.arma.Add;
////        AProd.arma[0].tpArma := TpcnTipoArma(rR01_4.FieldByName('R01_ARMAUSO').AsInteger);      // L02 - Indicador do tipo de arma de fogo // (0)=taUsoPermitido, (1)=taUsoRestrito
////        AProd.arma[0].nSerie := Copy(rR01_4.FieldByName('R01_ARMASERIE').AsString, 1, 9);     // L03 - Número de série da arma (tam. 1-9)
////        AProd.arma[0].nCano  := Copy(rR01_4.FieldByName('R01_ARMASERIECANO').AsString, 1, 9); // L04 - Número de série do cano (tam. 1-9)
////        AProd.arma[0].descr  := 'Registro : '     + rR01_4.FieldByName('R01_ARMAREGISTRO').AsString        + ' - ' + // L05 - Descrição completa da arma, compreendendo: calibre, marca, capacidade, etc) (tam. 1-256)
////                                'Sinarme : '      + rR01_4.FieldByName('R01_ARMASINARME').AsString         + ' - ' +
////                                'Espécie : '      + rR01_4.FieldByName('R01_ARMAESPECIE').AsString         + ' - ' +
////                                'Marca : '        + rR01_4.FieldByName('R01_ARMAMARCA').AsString           + ' - ' +
////                                'Calibre : '      + rR01_4.FieldByName('R01_ARMACALIBRE').AsString         + ' - ' +
////                                'Modelo : '       + rR01_4.FieldByName('R01_ARMAMODELO').AsString          + ' - ' +
////                                'Tamanho Cano : ' + rR01_4.FieldByName('R01_ARMATAMANHOCANO').AsString     + ' - ' +
////                                'Qtdade Cano : '  + rR01_4.FieldByName('R01_ARMAQUANTIDADECANO').AsString  + ' - ' +
////                                'Acabamento : '   + rR01_4.FieldByName('R01_ARMAACABAMENTO').AsString      + ' - ' +
////                                'Validade : '     + rR01_4.FieldByName('R01_ARMAVALIDADE').AsString        + ' - ' +
////                                'Tiros : '        + rR01_4.FieldByName('R01_ARMAQUANTIDADETIROS').AsString + ' - ' +
////                                'Pais : '         + rR01_4.FieldByName('R01_ARMAPAIS').AsString; //            + ' - ' +
////    //                                                     'Raia     : '     + rR01_4.FieldByName('R01_ARMARAIA').AsString            + ' / ' +
////    //                                                                         rR01_4.FieldByName('R01_ARMARAIAQUANTIDADE').AsString  + ' / ' +
////    //                                                                         rR01_4.FieldByName('R01_ARMARAIASENTIDO').AsString     + ' / ' +
////    //                                                                         rR01_4.FieldByName('R01_ARMARAIASISTEMA').AsString;
////        //
////        // Raiada
////        if rR01_4.FieldByName('R01_ARMARAIA').AsString = 'T' then
////        begin
////           sR01_RAIADA := 'Sim'                                           + ' - ' +
////                            rR01_4.FieldByName('R01_ARMARAIAQUANTIDADE').AsString + ' - ' +
////                     ifThen(rR01_4.FieldByName('R01_ARMARAIASENTIDO').AsInteger = 0, 'Direita', 'Esquerda') + ' - ' +
////                            rR01_4.FieldByName('R01_ARMARAIASISTEMA').AsString;
////        end
////        else
////           sR01_RAIADA := 'Nao';
////        //
////        FDFe.infAdProd := 'Numero: '    + rR01_4.FieldByName('R01_ARMASERIE').AsString           + ' ' +
////                          'Cano Tam.: ' + rR01_4.FieldByName('R01_ARMATAMANHOCANO').AsString     + ' ' +
////                          'Cano Qtde: ' + rR01_4.FieldByName('R01_ARMAQUANTIDADECANO').AsString  + ' ' +
////                          'Tiros: '     + rR01_4.FieldByName('R01_ARMAQUANTIDADETIROS').AsString + ' ' +
////                          'Pais: '      + rR01_4.FieldByName('R01_ARMAPAIS').AsString            + ' ' +
////                          'Raia: '      + sR01_RAIADA;
////     end;
////   finally
////   end;
end;

class function TNFeTagProdArma.New(ADFe: TACBrNFe): INFeImposto;
begin
  Result := Self.Create(ADFe);
end;

end.
