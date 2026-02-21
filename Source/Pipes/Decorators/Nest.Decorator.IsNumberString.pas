unit Nest.Decorator.IsNumberString;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsNumberStringAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsNumberStringAttribute }

constructor IsNumberStringAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsNumberString';
end;

function IsNumberStringAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsNumberString quando disponivel
  Result := nil;
end;

end.

