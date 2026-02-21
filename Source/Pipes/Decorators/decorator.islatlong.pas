unit decorator.islatlong;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsLatLongAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsLatLongAttribute }

constructor IsLatLongAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsLatLong';
end;

function IsLatLongAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsLatLong quando disponivel
  Result := nil;
end;

end.
