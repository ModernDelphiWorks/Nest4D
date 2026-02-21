unit nfebr.tag.di.model;

interface

uses
  Generics.Collections,
  nfebr.tag.adi.model;

type
  TDIModel = class
  private
    FCExportador: String;
    FCNPJ: String;
    FDDI: TDateTime;
    FDDesemb: TDateTime;
    FNDI: String;
    FTpIntermedio: integer;
    FTpViaTransp: integer;
    FUFDesemb: String;
    FUFTerceiro: String;
    FVAFRMM: Currency;
    FXLocDesemb: String;
    FAdi: TObjectList<TAdiModel>;
  public
    constructor Create;
    destructor Destroy; override;
    property CExportador: String read FCExportador write FCExportador;
    property CNPJ: String read FCNPJ write FCNPJ;
    property DDI: TDateTime read FDDI write FDDI;
    property DDesemb: TDateTime read FDDesemb write FDDesemb;
    property NDI: String read FNDI write FNDI;
    property TpIntermedio: integer read FTpIntermedio write FTpIntermedio;
    property TpViaTransp: integer read FTpViaTransp write FTpViaTransp;
    property UFDesemb: String read FUFDesemb write FUFDesemb;
    property UFTerceiro: String read FUFTerceiro write FUFTerceiro;
    property VAFRMM: Currency read FVAFRMM write FVAFRMM;
    property XLocDesemb: String read FXLocDesemb write FXLocDesemb;
    property Adi: TObjectList<TAdiModel> read FAdi write FAdi;
  end;

implementation

{ TDIModel }

constructor TDIModel.Create;
begin
  FAdi := TObjectList<TAdiModel>.Create;
end;

destructor TDIModel.Destroy;
begin
  FAdi.Free;
  inherited;
end;

end.

