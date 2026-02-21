unit decorator.isiso4217currencycode;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  isiso4217currencycodeAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ isiso4217currencycodeAttribute }

constructor isiso4217currencycodeAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'isiso4217currencycode';
end;

function isiso4217currencycodeAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao isiso4217currencycode quando disponivel
  Result := nil;
end;

end.
