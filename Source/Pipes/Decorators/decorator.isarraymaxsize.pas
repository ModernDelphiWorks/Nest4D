unit decorator.isarraymaxsize;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

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
