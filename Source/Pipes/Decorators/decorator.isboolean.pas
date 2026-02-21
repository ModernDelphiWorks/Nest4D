unit decorator.isboolean;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types,
  validation.Isboolean;

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






