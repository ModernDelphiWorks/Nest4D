unit Nest.Decorator.IsLongitude;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  islongitudeAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ islongitudeAttribute }

constructor islongitudeAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'islongitude';
end;

function islongitudeAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao islongitude quando disponivel
  Result := nil;
end;

end.

