unit Nest.Decorator.IsInstance;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

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

