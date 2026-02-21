unit decorator.isin;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsInAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsInAttribute }

constructor IsInAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsIn';
end;

function IsInAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsIn quando disponivel
  Result := nil;
end;

end.
