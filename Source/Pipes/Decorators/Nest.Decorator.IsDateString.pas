unit Nest.Decorator.IsDateString;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsdatestringAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsdatestringAttribute }

constructor IsdatestringAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Isdatestring';
end;

function IsdatestringAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao Isdatestring quando disponivel
  Result := nil;
end;

end.

