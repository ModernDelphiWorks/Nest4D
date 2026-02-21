unit Decorator.IsBase58;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsBase58Attribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

constructor IsBase58Attribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
end;

function IsBase58Attribute.Validation: TValidation;
begin
  // TODO: Implement IsBase58 validation logic
//  Result := TValidation.Create;
end;

end.