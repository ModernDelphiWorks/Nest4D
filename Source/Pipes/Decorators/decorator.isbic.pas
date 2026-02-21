unit decorator.isbic;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsBICAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

constructor IsBICAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
end;

function IsBICAttribute.Validation: TValidation;
begin
  // TODO: Implement IsBIC validation logic
//  Result := TValidation.Create;
end;

end.