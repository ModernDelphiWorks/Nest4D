unit Nest4D.Decorator.Body;

interface

uses
  System.SysUtils,
  System.Variants,
  Decorator.IsBase,
  Nest4D.Validation.Types,
  Nest4D.Parse.ArrayOf.Pipe;

type
  BodyAttribute = class(IsAttribute)
  protected
    FValue: Variant;
    FObjectType: TObjectType;
    FTransform: TTransform;
    FTransformAttribute: IsAttribute;
  public
    constructor Create(const AObjectType: TObjectType;
      const ATransform: TTransform; const AValue: Variant;
      const AValidation: TValidation = nil; const AMessage: String = '');
      reintroduce; overload;
    constructor Create(const AObjectType: TObjectType;
      const ATransform: TTransform; const AValidation: TValidation = nil;
      const AMessage: String = ''); reintroduce; overload;
    function TagName: String;
    function ObjectType: TObjectType;
    function Transform: TTransform;
    function Value: Variant;
  end;

implementation

{ BodyAttribute }

constructor BodyAttribute.Create(const AObjectType: TObjectType;
  const ATransform: TTransform; const AValue: Variant;
  const AValidation: TValidation; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'body';
  FTransform := ATransform;
  FValue := AValue;
  FObjectType := AObjectType;
end;

function BodyAttribute.TagName: String;
begin
  Result := FTagName;
end;

function BodyAttribute.Transform: TTransform;
begin
  Result := FTransform;
end;

function BodyAttribute.Value: Variant;
begin
  Result := FValue;
end;

constructor BodyAttribute.Create(const AObjectType: TObjectType;
  const ATransform: TTransform; const AValidation: TValidation;
  const AMessage: String);
begin
  Create(AObjectType, ATransform, Null, AValidation, AMessage);
end;

function BodyAttribute.ObjectType: TObjectType;
begin
  Result := FObjectType;
end;

end.





