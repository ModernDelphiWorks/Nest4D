unit decorator.isjwt;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsJWTAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsJWTAttribute }

constructor IsJWTAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsJWT';
end;

function IsJWTAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsJWT quando disponivel
  Result := nil;
end;

end.
