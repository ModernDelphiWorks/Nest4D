unit Nest.Decorator.IsRFC3339;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsRFC3339Attribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsRFC3339Attribute }

constructor IsRFC3339Attribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsRFC3339';
end;

function IsRFC3339Attribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsRFC3339 quando disponivel
  Result := nil;
end;

end.

