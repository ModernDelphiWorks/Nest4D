unit Decorator.IsAllow;

interface

uses
  System.SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.Types;

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




