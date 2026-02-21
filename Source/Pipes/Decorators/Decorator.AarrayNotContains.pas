unit Decorator.AarrayNotContains;

interface

uses
  System.SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types,
  Validation.ArrayNotContains;

type
  ArrayNotContainsAttribute = class(IsAttribute)
  private
    FValue: TArray<TValue>;
  public
    constructor Create(const AValue: TArray<TValue>;
      const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
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

function ArrayNotContainsAttribute.Validation: TValidation;
begin
  Result := TArrayNotContains;
end;

end.



