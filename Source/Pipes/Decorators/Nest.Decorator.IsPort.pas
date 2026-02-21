unit Nest.Decorator.IsPort;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsPortAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsPortAttribute }

constructor IsPortAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsPort';
end;

function IsPortAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsPort quando disponivel
  Result := nil;
end;

end.

