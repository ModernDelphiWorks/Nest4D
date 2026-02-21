unit decorator.isiso8601;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsISO8601Attribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsISO8601Attribute }

constructor IsISO8601Attribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsISO8601';
end;

function IsISO8601Attribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsISO8601 quando disponivel
  Result := nil;
end;

end.
