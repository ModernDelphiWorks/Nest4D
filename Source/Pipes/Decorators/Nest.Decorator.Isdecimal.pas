unit Nest.Decorator.Isdecimal;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  iscecimalAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ iscecimalAttribute }

constructor iscecimalAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'iscecimal';
end;

function iscecimalAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao iscecimal quando disponivel
  Result := nil;
end;

end.

