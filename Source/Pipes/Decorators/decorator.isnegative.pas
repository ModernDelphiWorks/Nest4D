unit decorator.isnegative;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsNegativeAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsNegativeAttribute }

constructor IsNegativeAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsNegative';
end;

function IsNegativeAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsNegative quando disponível
  Result := nil;
end;

end.


