unit decorator.isemail;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsEmailAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsEmailAttribute }

constructor IsEmailAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsEmail';
end;

function IsEmailAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação de email quando disponível
  Result := nil;
end;

end.


