unit decorator.ishexadecimal;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsHexadecimalAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsHexadecimalAttribute }

constructor IsHexadecimalAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsHexadecimal';
end;

function IsHexadecimalAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsHexadecimal quando disponível
  Result := nil;
end;

end.


