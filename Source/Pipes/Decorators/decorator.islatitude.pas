unit decorator.islatitude;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  islatitudeAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ islatitudeAttribute }

constructor islatitudeAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'islatitude';
end;

function islatitudeAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao islatitude quando disponivel
  Result := nil;
end;

end.
