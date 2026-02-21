unit decorator.isvariablewidth;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsVariableWidthAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsVariableWidthAttribute }

constructor IsVariableWidthAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsVariableWidth';
end;

function IsVariableWidthAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsVariableWidth quando disponivel
  Result := nil;
end;

end.
