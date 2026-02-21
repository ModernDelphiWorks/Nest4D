unit decorator.issurrogatepair;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsSurrogatePairAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsSurrogatePairAttribute }

constructor IsSurrogatePairAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsSurrogatePair';
end;

function IsSurrogatePairAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsSurrogatePair quando disponivel
  Result := nil;
end;

end.
