unit decorator.ismilitarytime;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsMilitaryTimeAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsMilitaryTimeAttribute }

constructor IsMilitaryTimeAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMilitaryTime';
end;

function IsMilitaryTimeAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsMilitaryTime quando disponivel
  Result := nil;
end;

end.
