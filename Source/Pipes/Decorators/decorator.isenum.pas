unit decorator.isenum;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types,
  validation.isenum;

type
  IsEnumAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsBooleanAttribute }

constructor IsEnumAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsEnum';
end;

function IsEnumAttribute.Validation: TValidation;
begin
  Result := TIsEnum;
end;

end.






