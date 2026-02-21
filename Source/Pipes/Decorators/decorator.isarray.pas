unit decorator.isarray;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types,
  validation.isarray;

type
  IsArrayAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsArrayAttribute }

constructor IsArrayAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsArray';
end;

function IsArrayAttribute.Validation: TValidation;
begin
  Result := TIsArray;
end;

end.






