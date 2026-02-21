unit decorator.ismatches;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

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
