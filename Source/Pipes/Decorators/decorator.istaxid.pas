unit decorator.istaxid;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsTaxIdAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsTaxIdAttribute }

constructor IsTaxIdAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsTaxId';
end;

function IsTaxIdAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsTaxId quando disponivel
  Result := nil;
end;

end.
