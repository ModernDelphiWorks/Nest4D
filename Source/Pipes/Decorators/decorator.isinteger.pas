unit decorator.isinteger;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types,
  validation.isinteger;

type
  IsIntegerAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsStringAttribute }

constructor IsIntegerAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsInteger';
end;

function IsIntegerAttribute.Validation: TValidation;
begin
  Result := TIsInteger;
end;

end.






