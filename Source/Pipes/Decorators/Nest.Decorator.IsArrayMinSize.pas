unit Nest.Decorator.IsArrayMinSize;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsArrayMinSizeAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

constructor IsArrayMinSizeAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
end;

function IsArrayMinSizeAttribute.Validation: TValidation;
begin
  // TODO: Implement IsArrayMinSize validation logic
//  Result := TValidation.Create;
end;

end.
