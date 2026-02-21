unit decorator.ishexcolor;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsHexColorAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsHexColorAttribute }

constructor IsHexColorAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsHexColor';
end;

function IsHexColorAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsHexColor quando disponivel
  Result := nil;
end;

end.
