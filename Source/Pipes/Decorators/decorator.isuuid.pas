unit decorator.isuuid;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsUUIDAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsUUIDAttribute }

constructor IsUUIDAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsUUID';
end;

function IsUUIDAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação de UUID quando disponível
  Result := nil;
end;

end.


