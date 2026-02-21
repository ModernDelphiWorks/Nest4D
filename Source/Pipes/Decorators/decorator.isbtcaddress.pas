unit decorator.isbtcaddress;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsBTCAddressAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsBTCAddressAttribute }

constructor IsBTCAddressAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsBTCAddress';
end;

function IsBTCAddressAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsBTCAddress quando disponivel
  Result := nil;
end;

end.
