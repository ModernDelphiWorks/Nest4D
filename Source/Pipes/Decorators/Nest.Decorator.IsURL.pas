unit Nest.Decorator.IsURL;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsURLAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsURLAttribute }

constructor IsURLAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsURL';
end;

function IsURLAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação de URL quando disponível
  Result := nil;
end;

end.



