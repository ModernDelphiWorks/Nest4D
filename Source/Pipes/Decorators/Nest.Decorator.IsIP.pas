unit Nest.Decorator.IsIP;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsIPAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsIPAttribute }

constructor IsIPAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsIP';
end;

function IsIPAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsIP quando disponivel
  Result := nil;
end;

end.

