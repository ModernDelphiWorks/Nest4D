unit Nest.Decorator.IsIn;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsInAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsInAttribute }

constructor IsInAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsIn';
end;

function IsInAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsIn quando disponivel
  Result := nil;
end;

end.

