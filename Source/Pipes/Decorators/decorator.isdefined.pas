unit decorator.isdefined;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsDefinedAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsDefinedAttribute }

constructor IsDefinedAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsDefined';
end;

function IsDefinedAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsDefined quando disponível
  Result := nil;
end;

end.


