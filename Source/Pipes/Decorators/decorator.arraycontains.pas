unit Decorator.ArrayContains;

interface

uses
  System.SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.Types,
  Validation.ArrayContains;

type
  ArrayContainsAttribute = class(IsAttribute)
  private
    FValue: TArray<TValue>;
  public
    constructor Create(const AValue: TArray<TValue>; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ ArrayContains }

constructor ArrayContainsAttribute.Create(const AValue: TArray<TValue>;
  const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'ArrayContains';
  FValue := AValue;
end;

function ArrayContainsAttribute.Params: TArray<TValue>;
begin
  Result := FValue;
end;

function ArrayContainsAttribute.Validation: TValidation;
begin
  Result := TArrayContains;
end;

end.








