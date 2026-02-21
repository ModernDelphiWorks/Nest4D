unit Nest.Decorator.IsOctal;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsOctalAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsOctalAttribute }

constructor IsOctalAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsOctal';
end;

function IsOctalAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsOctal quando disponivel
  Result := nil;
end;

end.

