unit decorator.isean;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IseanAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IseanAttribute }

constructor IseanAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Isean';
end;

function IseanAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao Isean quando disponivel
  Result := nil;
end;

end.
