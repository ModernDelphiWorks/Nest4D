unit Nest.Decorator.IsPhoneNumber;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsPhoneNumberAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsPhoneNumberAttribute }

constructor IsPhoneNumberAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsPhoneNumber';
end;

function IsPhoneNumberAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsPhoneNumber quando disponivel
  Result := nil;
end;

end.

