unit decorator.isissn;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsISSNAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsISSNAttribute }

constructor IsISSNAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsISSN';
end;

function IsISSNAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsISSN quando disponivel
  Result := nil;
end;

end.
