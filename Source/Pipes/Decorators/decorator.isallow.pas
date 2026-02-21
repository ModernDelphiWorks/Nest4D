unit decorator.isallow;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsAllowAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsArrayAttribute }

constructor IsAllowAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsAllow';
end;

function IsAllowAttribute.Validation: TValidation;
begin
//  Result := TIsArray;
end;

end.




