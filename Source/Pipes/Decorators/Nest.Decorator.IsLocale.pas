unit Nest.Decorator.IsLocale;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsLocaleAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsLocaleAttribute }

constructor IsLocaleAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsLocale';
end;

function IsLocaleAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsLocale quando disponivel
  Result := nil;
end;

end.

