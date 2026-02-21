unit Nest.Decorator.IsRGBColor;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsRGBColorAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsRGBColorAttribute }

constructor IsRGBColorAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsRGBColor';
end;

function IsRGBColorAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsRGBColor quando disponivel
  Result := nil;
end;

end.

