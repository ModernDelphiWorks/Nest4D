unit Nest.Decorator.IsIsrc;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsISRCAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsISRCAttribute }

constructor IsISRCAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsISRC';
end;

function IsISRCAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsISRC quando disponivel
  Result := nil;
end;

end.

