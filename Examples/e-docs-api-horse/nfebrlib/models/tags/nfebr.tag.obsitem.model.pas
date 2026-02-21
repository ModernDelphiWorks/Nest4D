unit nfebr.tag.obsitem.model;

interface

uses
  nfebr.tag.obscont.model,
  nfebr.tag.obsfisco.model;

type
  TObsItemModel = class
  private
    FObsCont: TObsContModel;
    FObsFisco: TObsFiscoModel;
  public
    constructor Create;
    destructor Destroy; override;
    property ObsCont: TObsContModel read FObsCont;
    property ObsFisco: TObsFiscoModel read FObsFisco;
  end;

implementation

{ TObsItemModel }

constructor TObsItemModel.Create;
begin
  FObsCont := TObsContModel.Create;
  FObsFisco := TObsFiscoModel.Create;
end;

destructor TObsItemModel.Destroy;
begin
  FObsCont.Free;
  FObsFisco.Free;
  inherited;
end;

end.
