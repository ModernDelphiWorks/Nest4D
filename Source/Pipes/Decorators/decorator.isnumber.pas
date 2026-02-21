unit decorator.isnumber;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types,
  Validation.isnumber;

type
  IsNumberAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsNumberAttribute }

constructor IsNumberAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsNumber';
end;

function IsNumberAttribute.Validation: TValidation;
begin
  Result := TIsNumber;
end;

end.






