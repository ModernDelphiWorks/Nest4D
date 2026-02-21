unit decorator.isequals;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsEqualsAttribute = class(IsAttribute)
  private
    FValue: TValue;
  public
    constructor Create(const AValue: String; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsEqualsAttribute }

constructor IsEqualsAttribute.Create(const AValue: String; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsEquals';
  FValue := AValue;
end;

function IsEqualsAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsEquals quando disponível
  Result := nil;
end;

function IsEqualsAttribute.Params: TArray<TValue>;
begin
  Result := [FValue];
end;

end.


