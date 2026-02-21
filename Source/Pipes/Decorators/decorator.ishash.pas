unit decorator.ishash;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IshashAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IshashAttribute }

constructor IshashAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Ishash';
end;

function IshashAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao Ishash quando disponivel
  Result := nil;
end;

end.
