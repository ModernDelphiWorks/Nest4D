unit decorator.isnotempty;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types,
  Validation.Isnotempty;

type
  IsnotemptyAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsnotemptyAttribute }

constructor IsnotemptyAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Isnotempty';
end;

function IsnotemptyAttribute.Validation: TValidation;
begin
  Result := TIsNotEmpty;
end;

end.






