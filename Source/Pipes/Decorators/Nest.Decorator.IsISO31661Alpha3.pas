unit Nest.Decorator.IsISO31661Alpha3;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsISO31661Alpha3Attribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsISO31661Alpha3Attribute }

constructor IsISO31661Alpha3Attribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsISO31661Alpha3';
end;

function IsISO31661Alpha3Attribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsISO31661Alpha3 quando disponivel
  Result := nil;
end;

end.

