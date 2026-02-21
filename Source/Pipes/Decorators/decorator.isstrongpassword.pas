unit decorator.isstrongpassword;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsStrongPasswordAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsStrongPasswordAttribute }

constructor IsStrongPasswordAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsStrongPassword';
end;

function IsStrongPasswordAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsStrongPassword quando disponivel
  Result := nil;
end;

end.
