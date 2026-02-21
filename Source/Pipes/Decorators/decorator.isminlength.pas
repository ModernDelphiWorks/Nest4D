unit decorator.isminlength;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types,
  validation.isminlength;

type
  IsMinLengthAttribute = class(IsAttribute)
  private
    FValueMin: TValue;
  public
    constructor Create(const AValueMax: Extended; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsMaxAttribute }

constructor IsMinLengthAttribute.Create(const AValueMax: Extended; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMinLength';
  FValueMin := AValueMax;
end;

function IsMinLengthAttribute.Params: TArray<TValue>;
begin
  Result := [FValueMin];
end;

function IsMinLengthAttribute.Validation: TValidation;
begin
  Result := TIsMinLength;
end;

end.




