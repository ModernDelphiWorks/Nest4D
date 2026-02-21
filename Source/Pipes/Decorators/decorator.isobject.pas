unit decorator.isobject;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types,
  Validation.isobject;

type
  IsObjectAttribute = class(IsAttribute)
  public
    constructor Create(const AValue: Extended; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
  end;

implementation

{ IsMaxAttribute }

constructor IsObjectAttribute.Create(const AValue: Extended; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsObject';
end;

function IsObjectAttribute.Validation: TValidation;
begin
  Result := TIsObject;
end;

end.






