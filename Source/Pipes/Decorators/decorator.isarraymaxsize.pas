unit Decorator.IsArrayMaxsize;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsArrayMaxSizeAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

constructor IsArrayMaxSizeAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
end;

function IsArrayMaxSizeAttribute.Validation: TValidation;
begin
  // TODO: Implement IsArrayMaxSize validation logic
//  Result := TValidation.Create;
end;

end.
