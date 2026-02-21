unit Nest.Decorator.IsIsin;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsISINAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsISINAttribute }

constructor IsISINAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsISIN';
end;

function IsISINAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsISIN quando disponivel
  Result := nil;
end;

end.

