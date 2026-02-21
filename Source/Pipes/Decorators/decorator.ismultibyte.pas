unit decorator.ismultibyte;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  IsMultibyteAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsMultibyteAttribute }

constructor IsMultibyteAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMultibyte';
end;

function IsMultibyteAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsMultibyte quando disponivel
  Result := nil;
end;

end.
