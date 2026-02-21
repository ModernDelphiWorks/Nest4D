unit decorator.isfirebasepushId;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types;

type
  isfirebasepushIdAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ isfirebasepushIdAttribute }

constructor isfirebasepushIdAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'isfirebasepushId';
end;

function isfirebasepushIdAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao isfirebasepushId quando disponivel
  Result := nil;
end;

end.
