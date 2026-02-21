unit nfebr.tag.vol.model;

interface

uses
  Generics.Collections,
  nfebr.tag.lacre.model;

type
  TVolModel = class
  private
    FEsp: String;
    FMarca: String;
    FNVol: String;
    FPesoB: Double;
    FPesoL: Double;
    FQVol: integer;
    FLacres: TObjectList<TLacresModel>;
  public
    constructor Create;
    destructor Destroy; override;
    property Esp: String read FEsp write FEsp;
    property Marca: String read FMarca write FMarca;
    property NVol: String read FNVol write FNVol;
    property PesoB: Double read FPesoB write FPesoB;
    property PesoL: Double read FPesoL write FPesoL;
    property QVol: integer read FQVol write FQVol;
    property Lacres: TObjectList<TLacresModel> read FLacres write FLacres;
  end;

implementation

{ TVolModel }

constructor TVolModel.Create;
begin
  FLacres := TObjectList<TLacresModel>.Create;
end;

destructor TVolModel.Destroy;
begin
  FLacres.Free;
  inherited;
end;

end.
