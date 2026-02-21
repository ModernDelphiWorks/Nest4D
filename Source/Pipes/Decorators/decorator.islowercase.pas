unit decorator.islowercase;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsLowercaseAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsLowercaseAttribute }

constructor IsLowercaseAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsLowercase';
end;

function IsLowercaseAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsLowercase quando disponível
  Result := nil;
end;

end.


