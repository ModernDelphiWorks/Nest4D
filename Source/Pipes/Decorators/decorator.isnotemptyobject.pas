unit decorator.isnotemptyobject;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsNotEmptyObjectAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsNotEmptyObjectAttribute }

constructor IsNotEmptyObjectAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsNotEmptyObject';
end;

function IsNotEmptyObjectAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsNotEmptyObject quando disponivel
  Result := nil;
end;

end.
