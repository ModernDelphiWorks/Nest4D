unit decorator.isdivisibleby;

interface

uses
  SysUtils,
  decorator.isbase,
  nest4d.validation.types;

type
  IsDivisibleByAttribute = class(IsAttribute)
  private
    FValue: TValue;
  public
    constructor Create(const AValue: Extended; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsDivisibleByAttribute }

constructor IsDivisibleByAttribute.Create(const AValue: Extended; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsDivisibleBy';
  FValue := AValue;
end;

function IsDivisibleByAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsDivisibleBy quando disponível
  Result := nil;
end;

function IsDivisibleByAttribute.Params: TArray<TValue>;
begin
  Result := [FValue];
end;

end.


