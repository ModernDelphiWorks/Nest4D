unit decorator.isascii;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsASCIIAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

constructor IsASCIIAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
end;

function IsASCIIAttribute.Validation: TValidation;
begin
  // TODO: Implement IsASCII validation logic
//  Result := TValidation.Create;
end;

end.