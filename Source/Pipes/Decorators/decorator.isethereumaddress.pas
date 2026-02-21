unit decorator.isethereumaddress;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsEthereumAddressAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsEthereumAddressAttribute }

constructor IsEthereumAddressAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsEthereumAddress';
end;

function IsEthereumAddressAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsEthereumAddress quando disponivel
  Result := nil;
end;

end.
