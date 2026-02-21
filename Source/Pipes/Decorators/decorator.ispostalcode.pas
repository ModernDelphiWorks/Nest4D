unit decorator.ispostalcode;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsPostalCodeAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsPostalCodeAttribute }

constructor IsPostalCodeAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsPostalCode';
end;

function IsPostalCodeAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsPostalCode quando disponivel
  Result := nil;
end;

end.
