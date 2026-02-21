unit decorator.islength;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types,
  validation.IsLength;

type
  IsLengthAttribute = class(IsAttribute)
  private
    FValueMin: TValue;
    FValueMax: TValue;
  public
    constructor Create(const AValueMin: Extended; const AValueMax: Extended;
      const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsLengthAttribute }

constructor IsLengthAttribute.Create(const AValueMin: Extended;
  const AValueMax: Extended; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsLength';
  FValueMin := AValueMin;
  FValueMax := AValueMax;
end;

function IsLengthAttribute.Params: TArray<TValue>;
begin
  Result := [FValueMin, FValueMax];
end;

function IsLengthAttribute.Validation: TValidation;
begin
  Result := TIsLength;
end;

end.




