unit nfebr.tag.pag.model;

interface

uses
  Generics.Collections,
  nfebr.tag.detpag.model;

type
  TPagModel = class
  private
    FDetPag: TObjectList<TDetPagModel>;
    FVTroco: Currency;
  public
    constructor Create;
    destructor Destroy; override;
    property DetPag: TObjectList<TDetPagModel> read FDetPag;
    property VTroco: Currency read FVTroco write FVTroco;
  end;

implementation

{ TPagModel }

constructor TPagModel.Create;
begin
  FDetPag := TObjectList<TDetPagModel>.Create;
end;

destructor TPagModel.Destroy;
begin
  FDetPag.Free;
  inherited;
end;

end.

