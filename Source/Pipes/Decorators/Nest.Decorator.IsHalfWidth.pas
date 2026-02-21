unit Nest.Decorator.IsHalfWidth;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsHalfWidthAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsHalfWidthAttribute }

constructor IsHalfWidthAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsHalfWidth';
end;

function IsHalfWidthAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsHalfWidth quando disponivel
  Result := nil;
end;

end.

