unit Nest4D.Decorator.Query;

interface

uses
  System.SysUtils,
  System.Variants,
  Decorator.IsBase,
  Nest4D.Validation.Types;

type
  QueryAttribute = class(IsAttribute)
  private
    FValue: Variant;
    FQueryName: String;
    FTransform: TTransform;
    FValidation: TValidation;
  public
    constructor Create(const AQueryName: String; const ATransform: TTransform;
      const AValue: Variant; const AValidation: TValidation = nil;
      const AMessage: String = ''); reintroduce; overload;
    constructor Create(const AQueryName: String; const ATransform: TTransform;
      const AValidation: TValidation = nil; const AMessage: String = '');
      reintroduce; overload;
    function QueryName: String;
    function TagName: String;
    function Transform: TTransform;
    function Value: Variant;
    function validation: TValidation; override;
  end;

implementation

uses
  Nest4D.Transform.pipe;

{ ParamAttribute }

function QueryAttribute.Transform: TTransform;
begin
  Result := FTransform;
end;

constructor QueryAttribute.Create(const AQueryName: String;
  const ATransform: TTransform; const AValidation: TValidation;
  const AMessage: String);
begin
  Create(AQueryName, ATransform, Null, AValidation, AMessage);
end;

constructor QueryAttribute.Create(const AQueryName: String;
  const ATransform: TTransform; const AValue: Variant;
  const AValidation: TValidation; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'query';
  FValue := AValue;
  FQueryName := AQueryName;
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

function QueryAttribute.QueryName: String;
begin
  Result := FQueryName;
end;

function QueryAttribute.TagName: String;
begin
  Result := FTagName;
end;

function QueryAttribute.validation: TValidation;
begin
  Result := FValidation;
end;

function QueryAttribute.Value: Variant;
begin

end;

end.








