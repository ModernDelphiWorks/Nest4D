unit decorator.isboolean;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types,
  Validation.Isboolean;

type
  IsbooleanAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsbooleanAttribute }

constructor IsbooleanAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Isboolean';
end;

function IsbooleanAttribute.Validation: TValidation;
begin
  Result := TIsBoolean;
end;

end.






