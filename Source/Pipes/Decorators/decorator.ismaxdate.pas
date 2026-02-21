unit decorator.ismaxdate;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsMaxDateAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsMaxDateAttribute }

constructor IsMaxDateAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMaxDate';
end;

function IsMaxDateAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsMaxDate quando disponivel
  Result := nil;
end;

end.
