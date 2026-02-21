unit decorator.issemver;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsSemVerAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsSemVerAttribute }

constructor IsSemVerAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsSemVer';
end;

function IsSemVerAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsSemVer quando disponivel
  Result := nil;
end;

end.
