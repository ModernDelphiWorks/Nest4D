unit decorator.ismacaddress;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsMACAddressAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsMACAddressAttribute }

constructor IsMACAddressAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMACAddress';
end;

function IsMACAddressAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsMACAddress quando disponivel
  Result := nil;
end;

end.
