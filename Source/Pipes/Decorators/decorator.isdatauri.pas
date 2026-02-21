unit decorator.isdatauri;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsdatauriAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsdatauriAttribute }

constructor IsdatauriAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Isdatauri';
end;

function IsdatauriAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao Isdatauri quando disponivel
  Result := nil;
end;

end.
