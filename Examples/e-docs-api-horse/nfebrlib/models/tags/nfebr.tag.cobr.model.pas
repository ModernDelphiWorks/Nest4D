unit nfebr.tag.cobr.model;

interface

uses
  Generics.Collections,
  nfebr.tag.dup.model,
  nfebr.tag.fat.model;

type
  TCobrModel = class
  private
    FFat: TFatModel;
    FDup: TObjectList<TDupModel>;
  public
    constructor Create;
    destructor Destroy; override;
    property Fat: TFatModel read FFat;
    property Dups: TObjectList<TDupModel> read FDup write FDup;
  end;

implementation

{ TCobrModel }

constructor TCobrModel.Create;
begin
  FDup := TObjectList<TDupModel>.Create;
end;

destructor TCobrModel.Destroy;
begin
  FDup.Free;
  inherited;
end;

end.

