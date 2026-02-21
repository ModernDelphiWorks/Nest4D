unit Nest.Decorator.IsPositive;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsPositiveAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsPositiveAttribute }

constructor IsPositiveAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsPositive';
end;

function IsPositiveAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsPositive quando disponível
  Result := nil;
end;

end.



