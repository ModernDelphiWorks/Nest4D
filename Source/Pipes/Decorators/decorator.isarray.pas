unit Decorator.IsArray;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types,
  Validation.isarray;

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






