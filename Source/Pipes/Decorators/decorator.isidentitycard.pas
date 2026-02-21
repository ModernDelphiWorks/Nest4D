unit decorator.isidentitycard;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsIdentityCardAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsIdentityCardAttribute }

constructor IsIdentityCardAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsIdentityCard';
end;

function IsIdentityCardAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsIdentityCard quando disponivel
  Result := nil;
end;

end.
