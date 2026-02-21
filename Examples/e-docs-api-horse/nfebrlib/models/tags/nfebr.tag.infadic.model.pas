unit nfebr.tag.infadic.model;

interface

uses
  Classes,
  Generics.Collections,
  nfebr.tag.obscont.model,
  nfebr.tag.obsfisco.model,
  nfebr.tag.procref.model;

type
  TInfAdicModel = class
  private
    FInfAdFisco: String;
    FInfCpl: String;
    FObsCont: TObjectList<TObsContModel>;
    FObsFisco: TObjectList<TObsFiscoModel>;
    FProcRef: TObjectList<TProcRefModel>;
    function GetObsCont: TObjectList<TObsContModel>;
    function GetObsFisco: TObjectList<TObsFiscoModel>;
    function GetProcRef: TObjectList<TProcRefModel>;
  public
    constructor Create;
    destructor Destroy; override;
    property InfAdFisco: String read FInfAdFisco write FInfAdFisco;
    property InfCpl: String read FInfCpl write FInfCpl;
    property ObsCont: TObjectList<TObsContModel> read GetObsCont;
    property ObsFisco: TObjectList<TObsFiscoModel> read GetObsFisco;
    property ProcRef: TObjectList<TProcRefModel> read GetProcRef;
  end;

implementation

{ TInfAdicModel }

constructor TInfAdicModel.Create;
begin
  FObsCont := TObjectList<TObsContModel>.Create;
  FObsFisco := TObjectList<TObsFiscoModel>.Create;
  FProcRef := TObjectList<TProcRefModel>.Create;
end;

destructor TInfAdicModel.Destroy;
begin
  FObsCont.Free;
  FObsFisco.Free;
  FProcRef.Free;
  inherited;
end;

function TInfAdicModel.GetObsCont: TObjectList<TObsContModel>;
begin
  Result := FObsCont;
end;

function TInfAdicModel.GetObsFisco: TObjectList<TObsFiscoModel>;
begin
  Result := FObsFisco;
end;

function TInfAdicModel.GetProcRef: TObjectList<TProcRefModel>;
begin
  Result := FProcRef;
end;

end.

