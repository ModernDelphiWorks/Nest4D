unit Nest.Decorator.IsMobilePhone;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsMobilePhoneAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsMobilePhoneAttribute }

constructor IsMobilePhoneAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMobilePhone';
end;

function IsMobilePhoneAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsMobilePhone quando disponivel
  Result := nil;
end;

end.

