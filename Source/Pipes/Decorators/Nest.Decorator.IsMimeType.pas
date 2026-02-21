unit Nest.Decorator.IsMimeType;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsMimeTypeAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsMimeTypeAttribute }

constructor IsMimeTypeAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMimeType';
end;

function IsMimeTypeAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsMimeType quando disponivel
  Result := nil;
end;

end.

