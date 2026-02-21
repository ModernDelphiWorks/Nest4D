unit Nest.Decorator.IsNotEquals;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types;

type
  IsNotEqualsAttribute = class(IsAttribute)
  private
    FValue: TValue;
  public
    constructor Create(const AValue: String; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsNotEqualsAttribute }

constructor IsNotEqualsAttribute.Create(const AValue: String; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsNotEquals';
  FValue := AValue;
end;

function IsNotEqualsAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsNotEquals quando disponível
  Result := nil;
end;

function IsNotEqualsAttribute.Params: TArray<TValue>;
begin
  Result := [FValue];
end;

end.



