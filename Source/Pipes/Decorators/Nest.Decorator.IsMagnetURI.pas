unit Nest.Decorator.IsMagnetURI;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  ismagneturiAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ ismagneturiAttribute }

constructor ismagneturiAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'ismagneturi';
end;

function ismagneturiAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao ismagneturi quando disponivel
  Result := nil;
end;

end.

