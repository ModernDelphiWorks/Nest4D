unit decorator.isport;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

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
