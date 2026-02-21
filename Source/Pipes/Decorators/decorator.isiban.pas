unit decorator.isiban;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsIBANAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsIBANAttribute }

constructor IsIBANAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsIBAN';
end;

function IsIBANAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsIBAN quando disponivel
  Result := nil;
end;

end.
