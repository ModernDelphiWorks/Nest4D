unit Nest.Decorator.ArrayNotContains;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types,
  validation.arraynotcontains;

type
  ArrayNotContainsAttribute = class(IsAttribute)
  private
    FValue: TArray<TValue>;
  public
    constructor Create(const AValue: TArray<TValue>;
      const AMessage: String = ''); reintroduce;
    function validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ ArrayContains }

constructor ArrayNotContainsAttribute.Create(const AValue: TArray<TValue>;
  const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'ArrayNotContains';
  FValue := AValue;
end;

function ArrayNotContainsAttribute.Params: TArray<TValue>;
begin
  Result := FValue;
end;

function ArrayNotContainsAttribute.validation: TValidation;
begin
  Result := TArrayNotContains;
end;

end.


