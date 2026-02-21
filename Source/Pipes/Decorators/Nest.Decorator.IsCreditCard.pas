unit Nest.Decorator.IsCreditCard;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsCreditCardAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsCreditCardAttribute }

constructor IsCreditCardAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsCreditCard';
end;

function IsCreditCardAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsCreditCard quando disponivel
  Result := nil;
end;

end.

