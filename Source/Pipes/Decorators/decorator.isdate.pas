unit decorator.isdate;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types,
  Validation.Isdate;

type
  IsdateAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsBooleanAttribute }

constructor IsdateAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Isdate';
end;

function IsdateAttribute.Validation: TValidation;
begin
  Result := TIsDate;
end;

end.






