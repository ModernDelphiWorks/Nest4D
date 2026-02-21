unit decorator.isuppercase;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsUppercaseAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsUppercaseAttribute }

constructor IsUppercaseAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsUppercase';
end;

function IsUppercaseAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsUppercase quando disponível
  Result := nil;
end;

end.


