unit decorator.isisin;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsISINAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsISINAttribute }

constructor IsISINAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsISIN';
end;

function IsISINAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsISIN quando disponivel
  Result := nil;
end;

end.
