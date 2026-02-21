unit Nest.Decorator.IsFullWidth;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsFullWidthAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsFullWidthAttribute }

constructor IsFullWidthAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsFullWidth';
end;

function IsFullWidthAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsFullWidth quando disponivel
  Result := nil;
end;

end.

