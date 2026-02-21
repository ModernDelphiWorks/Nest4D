unit Nest.Decorator.IsHsl;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsHSLAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsHSLAttribute }

constructor IsHSLAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsHSL';
end;

function IsHSLAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsHSL quando disponivel
  Result := nil;
end;

end.

