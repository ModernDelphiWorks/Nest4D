unit Nest.Decorator.IsEmpty;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types,
  validation.Isempty;

type
  IsemptyAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsemptyAttribute }

constructor IsemptyAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Isempty';
end;

function IsemptyAttribute.Validation: TValidation;
begin
  Result := TIsEmpty;
end;

end.







