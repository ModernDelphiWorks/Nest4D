unit Nest.Decorator.Param;

interface

uses
  SysUtils,
  Variants,
  decorator.isbase,
  Nest.validation.types;

type
  ParamAttribute = class(IsAttribute)
  private
    FValue: Variant;
    FParamName: String;
    FTransform: TTransform;
    FValidation: TValidation;
  public
    constructor Create(const AParamName: String; const ATransform: TTransform;
      const AValue: Variant; const AValidation: TValidation = nil;
      const AMessage: String = ''); reintroduce; overload;
    constructor Create(const AParamName: String; const ATransform: TTransform;
      const AValidation: TValidation = nil; const AMessage: String = '');
      reintroduce; overload;
    function ParamName: String;
    function TagName: String;
    function Transform: TTransform;
    function Value: Variant;
    function validation: TValidation; override;
  end;

implementation

uses
  Nest.transform.pipe;

{ ParamAttribute }

function ParamAttribute.Transform: TTransform;
begin
  Result := FTransform;
end;

constructor ParamAttribute.Create(const AParamName: String;
  const ATransform: TTransform; const AValue: Variant;
  const AValidation: TValidation; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'param';
  FParamName := AParamName;
  FValue := AValue;
  if (ATransform <> nil) and (AValidation <> nil) then
  begin
    FTransform := ATransform;
    FValidation := AValidation;
  end
  else
  begin
    if ATransform.InheritsFrom(TTransformPipe) then
      FTransform := ATransform
    else
      FValidation := ATransform;
  end;
end;

constructor ParamAttribute.Create(const AParamName: String;
  const ATransform: TTransform; const AValidation: TValidation;
  const AMessage: String);
begin
  Create(AParamName, ATransform, Null, AValidation, AMessage);
end;

function ParamAttribute.ParamName: String;
begin
  Result := FParamName;
end;

function ParamAttribute.TagName: String;
begin
  Result := FTagName;
end;

function ParamAttribute.validation: TValidation;
begin
  Result := FValidation;
end;

function ParamAttribute.Value: Variant;
begin
  Result := FValue;
end;

end.


