unit Nest.Decorator.IsPassportNumber;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsPassportNumberAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsPassportNumberAttribute }

constructor IsPassportNumberAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsPassportNumber';
end;

function IsPassportNumberAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsPassportNumber quando disponivel
  Result := nil;
end;

end.

