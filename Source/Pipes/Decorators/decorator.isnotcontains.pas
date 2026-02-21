unit decorator.isnotcontains;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsNotContainsAttribute = class(IsAttribute)
  private
    FValue: TValue;
  public
    constructor Create(const AValue: String; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsNotContainsAttribute }

constructor IsNotContainsAttribute.Create(const AValue: String; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsNotContains';
  FValue := AValue;
end;

function IsNotContainsAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsNotContains quando disponível
  Result := nil;
end;

function IsNotContainsAttribute.Params: TArray<TValue>;
begin
  Result := [FValue];
end;

end.


