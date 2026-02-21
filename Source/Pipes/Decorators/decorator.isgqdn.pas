unit decorator.isgqdn;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  isgqdnAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ isgqdnAttribute }

constructor isgqdnAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'isgqdn';
end;

function isgqdnAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao isgqdn quando disponivel
  Result := nil;
end;

end.
