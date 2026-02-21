unit Nest.Decorator.IsByteLength;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  isbytelengthAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ isbytelengthAttribute }

constructor isbytelengthAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'isbytelength';
end;

function isbytelengthAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao isbytelength quando disponivel
  Result := nil;
end;

end.

