unit decorator.isiso31661alpha2;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsISO31661Alpha2Attribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsISO31661Alpha2Attribute }

constructor IsISO31661Alpha2Attribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsISO31661Alpha2';
end;

function IsISO31661Alpha2Attribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsISO31661Alpha2 quando disponivel
  Result := nil;
end;

end.
