unit Nest.Decorator.IsMinDate;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsMinDateAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsMinDateAttribute }

constructor IsMinDateAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMinDate';
end;

function IsMinDateAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsMinDate quando disponivel
  Result := nil;
end;

end.

