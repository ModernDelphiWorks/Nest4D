unit decorator.ismax;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types,
  Validation.ismax;

type
  IsMaxAttribute = class(IsAttribute)
  private
    FValueMax: TValue;
  public
    constructor Create(const AValueMax: Extended; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsMaxAttribute }

constructor IsMaxAttribute.Create(const AValueMax: Extended; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMax';
  FValueMax := AValueMax;
end;

function IsMaxAttribute.Params: TArray<TValue>;
begin
  Result := [FValueMax];
end;

function IsMaxAttribute.Validation: TValidation;
begin
  Result := TIsMax;
end;

end.





