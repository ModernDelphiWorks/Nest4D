unit decorator.isnotin;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsNotInAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsNotInAttribute }

constructor IsNotInAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsNotIn';
end;

function IsNotInAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsNotIn quando disponivel
  Result := nil;
end;

end.
