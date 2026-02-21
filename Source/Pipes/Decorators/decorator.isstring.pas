unit decorator.isstring;

interface

uses
  SysUtils,
  Decorator.IsBase,
  Nest4D.Validation.types,
  Validation.isString;

type
  IsStringAttribute = class(IsAttribute)
  private
    FEach: Boolean;
    FGroups: TArray<String>;
  public
    constructor Create(const AEach: Boolean = False;
      const AMessage: String = ''; const AGroups: TArray<String> = nil); reintroduce;
    function Validation: TValidation; override;
  end;

implementation

{ IsStringAttribute }

constructor IsStringAttribute.Create(const AEach: Boolean;
  const AMessage: String; const AGroups: TArray<String>);
begin
  inherited Create(AMessage);
  FTagName := 'IsString';
  FEach := AEach;
  FGroups := AGroups;
end;

function IsStringAttribute.Validation: TValidation;
begin
  Result := TIsString;
end;

end.






