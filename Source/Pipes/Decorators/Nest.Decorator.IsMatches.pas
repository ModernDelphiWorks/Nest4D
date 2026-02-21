unit Nest.Decorator.IsMatches;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsMatchesAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsMatchesAttribute }

constructor IsMatchesAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMatches';
end;

function IsMatchesAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsMatches quando disponivel
  Result := nil;
end;

end.

