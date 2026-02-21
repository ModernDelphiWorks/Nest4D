unit decorator.isinstance;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  isinstanceAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ isinstanceAttribute }

constructor isinstanceAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'isinstance';
end;

function isinstanceAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao isinstance quando disponivel
  Result := nil;
end;

end.
