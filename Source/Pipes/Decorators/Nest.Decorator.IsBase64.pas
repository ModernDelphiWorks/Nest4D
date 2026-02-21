unit Nest.Decorator.IsBase64;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsBase64Attribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

constructor IsBase64Attribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
end;

function IsBase64Attribute.Validation: TValidation;
begin
  // TODO: Implement IsBase64 validation logic
//  Result := TValidation.Create;
end;

end.
