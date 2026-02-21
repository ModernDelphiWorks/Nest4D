unit decorator.ismin;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types,
  Validation.ismin;

type
  IsMinAttribute = class(IsAttribute)
  private
    FValueMin: TValue;
  public
    constructor Create(const AValueMin: Extended; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsMinAttribute }

constructor IsMinAttribute.Create(const AValueMin: Extended; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMin';
  FValueMin := AValueMin;
end;

function IsMinAttribute.Params: TArray<TValue>;
begin
  Result := [FValueMin];
end;

function IsMinAttribute.Validation: TValidation;
begin
  Result := TIsMin;
end;

end.




