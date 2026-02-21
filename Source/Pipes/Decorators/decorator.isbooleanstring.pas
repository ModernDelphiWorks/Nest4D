unit decorator.isbooleanstring;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsBooleanStringAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

constructor IsBooleanStringAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
end;

function IsBooleanStringAttribute.Validation: TValidation;
begin
  // TODO: Implement IsBooleanString validation logic
//  Result := TValidation.Create;
end;

end.