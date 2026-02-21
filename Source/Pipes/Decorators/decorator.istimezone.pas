unit decorator.istimezone;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsTimezoneAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsTimezoneAttribute }

constructor IsTimezoneAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsTimezone';
end;

function IsTimezoneAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsTimezone quando disponivel
  Result := nil;
end;

end.
