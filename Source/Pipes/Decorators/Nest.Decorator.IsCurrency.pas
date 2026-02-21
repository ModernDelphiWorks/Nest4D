unit Nest.Decorator.IsCurrency;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IscurrencyAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IscurrencyAttribute }

constructor IscurrencyAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Iscurrency';
end;

function IscurrencyAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao Iscurrency quando disponivel
  Result := nil;
end;

end.

