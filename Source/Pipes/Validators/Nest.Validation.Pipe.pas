{
             Nest4D - Development Framework for Delphi


                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers?o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos ? permitido copiar e distribuir c?pias deste documento de
       licen?a, mas mud?-lo n?o ? permitido.

       Esta vers?o da GNU Lesser General Public License incorpora
       os termos e condi??es da vers?o 3 da GNU General Public License
       Licen?a, complementado pelas permiss?es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework for Delphi)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://nest4d-en.docs-br.com)
}

unit Nest.Validation.Pipe;

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  StrUtils,
  Generics.Collections,
  System.Evolution.ResultPair,
  System.Evolution.Objects,
  System.Evolution.Std,
  Nest.route.handler,
  decorator.include,
  Nest.validation.include,
  Nest.validation.interfaces,
  Nest.transform.interfaces,
  Nest.request;

type
  TValidations = class(TList<IValidationInfo>);
  TTransforms = class(TList<ITransformInfo>);

  TValidationInfo = class(TInterfacedObject, IValidationInfo)
  private
    FValue: TValue;
    FValidationPipe: IValidatorConstraint;
    FValidationArguments: IValidationArguments;
    function _GetValidator: IValidatorConstraint;
    function _GetValidationArguments: IValidationArguments;
    function _GetValue: TValue;
    procedure _SetValidator(const Value: IValidatorConstraint);
    procedure _SetValidationArguments(const Value: IValidationArguments);
    procedure _SetValue(const Value: TValue);
  end;

  TTransformInfo = class(TInterfacedObject, ITransformInfo)
  private
    FValue: TValue;
    FConvertPipe: ITransformPipe;
    FConvertArguments: ITransformArguments;
    function _GetTransform: ITransformPipe;
    function _GetTransformArguments: ITransformArguments;
    function _GetValue: TValue;
    procedure _SetTransform(const Value: ITransformPipe);
    procedure _SetTransformArguments(const Value: ITransformArguments);
    procedure _SetValue(const Value: TValue);
  end;

  TValidationPipe = class(TInterfacedObject, IValidationPipe)
  private
    FContext: TRttiContext;
    FValidations: TValidations;
    FTransforms: TTransforms;
    FMessages: TSmartPtr<TList<String>>;
    FJsonMapped: TJsonMapped;
    procedure _MapPipes(const AClass: TClass; const ARequest: IRouteRequest); inline;
    procedure _MapValidation(const AClass: TClass; const ARequest: IRouteRequest); inline;
    procedure _ResolveParams(const ADecorator: TCustomAttribute; const ARequest: IRouteRequest); inline;
    procedure _ResolveQuerys(const ADecorator: TCustomAttribute; const ARequest: IRouteRequest); inline;
    procedure _ResolvePipes(const AClass: TClass; const ARttiType: TRttiType;
      const ARequest: IRouteRequest); inline;
    procedure _ResolvePayLoads(const ARttiType: TRttiType;
      const ARequest: IRouteRequest); inline;
    procedure _ResolveBody(const ADecorator: TCustomAttribute; const ARequest: IRouteRequest);
    function _ArrayMerge<T>(const AArray1: TArray<T>; const AArray2: TArray<T>): TArray<T>; inline;
  public
    constructor Create;
    destructor Destroy; override;
    function IsMessages: Boolean; inline;
    function BuildMessages: String; inline;
    procedure Validate(const AClass: TClass; const ARequest: IRouteRequest);
  end;

implementation

{ TValidationPipe }

constructor TValidationPipe.Create;
begin
  FContext := TRttiContext.Create;
  FMessages := TList<String>.Create;
end;

destructor TValidationPipe.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TValidationPipe.IsMessages: Boolean;
begin
  Result := False;
  if FMessages.AsRef = nil then
    exit;
  Result := FMessages.AsRef.Count > 0;
end;

procedure TValidationPipe.Validate(const AClass: TClass;
  const ARequest: IRouteRequest);
var
  LValidator: IValidationInfo;
  LInfo: ITransformInfo;
  LResultTransform: TResultTransform;
  LResultValidation: TResultValidation;
begin
  FMessages.AsRef.Clear;
  FJsonMapped := TJsonMapped.Create([doOwnsValues]);
  FValidations := TValidations.Create;
  FTransforms := TTransforms.Create;
  { TODO -oIsaque -cPerformance : Implementar threads nos FORs }
  try
    _MapValidation(AClass, ARequest);
    // Transforms
    for LInfo in FTransforms do
    begin
      LResultTransform := LInfo.Transform.Transform(LInfo.Value,
                                                    LInfo.Metadata);
      LResultTransform.When(
        procedure(Value: TValue)
        begin
          if LInfo.Metadata.TagName = 'body' then
          begin
            if Value.IsObject then
              ARequest.SetObject(Value.AsType<TObject>)
            else
              ARequest.SetBody(Value.AsType<String>);
          end
          else
          if LInfo.Metadata.TagName = 'param' then
            ARequest.Params.AddOrSetValue(LInfo.Metadata.FieldName, Value)
          else
          if LInfo.Metadata.TagName = 'query' then
            ARequest.Querys.AddOrSetValue(LInfo.Metadata.FieldName, Value);
        end,
        procedure(Msg: String)
        begin
          FMessages.AsRef.Add(Msg);
        end);
    end;
    // Validations
    for LValidator in FValidations do
    begin
      LResultValidation := LValidator.Validator.Validate(LValidator.Value,
                                                         LValidator.Args);
      LResultValidation.When(
        procedure(Value: Boolean)
        begin

        end,
        procedure(Msg: String)
        begin
          FMessages.AsRef.Add(Msg);
        end);
    end;
  finally
    FJsonMapped.Free;
    FValidations.Free;
    FTransforms.Free;
  end;
end;

procedure TValidationPipe._ResolveBody(const ADecorator: TCustomAttribute;
  const ARequest: IRouteRequest);
var
  LBody: BodyAttribute;
  LTransform: ITransformInfo;
  LValue: TValue;
  LResultBody: TResultTransform;
  LObject: IEvolutionObject;
begin
  LBody := BodyAttribute(ADecorator);
  LValue := ARequest.Body;
  if LBody.Transform <> nil then
  begin
    if LBody.Transform.InheritsFrom(TParseJsonPipe) then
    begin
      LObject := TEvolutionObject.New;
      // Transform
      LTransform := TTransformInfo.Create;
      LTransform.Transform := LObject.Factory(LBody.Transform) as TParseJsonPipe;
      LTransform.Value := LValue;
      LTransform.Metadata := TTransformArguments.Create([TValue.FromVariant(LBody.Value)],
                                                        LBody.TagName,
                                                        'body',
                                                        LBody.Message,
                                                        LBody.ObjectType);
      LResultBody := LTransform.Transform
                               .Transform(LValue, LTransform.Metadata);
      LResultBody.When(
        procedure(Value: TValue)
        var
          LItem: TPair<String, TList<TValue>>;
        begin
          for LItem in Value.AsType<TJsonMapped> do
            FJsonMapped.AddOrSetValue(LItem.Key, TList<TValue>.Create(LItem.Value));
        end,
        procedure(Msg: String)
        begin
          FMessages.AsRef.Add(Msg);
          exit;
        end);
    end
    else
    begin
      if LBody.Transform <> nil then
      begin
        LTransform := TTransformInfo.Create;
        LTransform.Transform := LBody.Transform.Create as TTransformPipe;
        LTransform.Value := LValue;
        LTransform.Metadata := TTransformArguments.Create([TValue.FromVariant(LBody.Value)],
                                                          LBody.TagName,
                                                          'body',
                                                          LBody.Message,
                                                          LBody.ObjectType);
        FTransforms.Add(LTransform);
      end;
    end;
  end;
  _MapPipes(LBody.ObjectType, ARequest);
end;

procedure TValidationPipe._ResolveParams(const ADecorator: TCustomAttribute;
  const ARequest: IRouteRequest);
var
  LValue: TValue;
  LParam: ParamAttribute;
  LTransform: ITransformInfo;
  LValidation: IValidationInfo;
begin
  LParam := ParamAttribute(ADecorator);
  LValue := IfThen(ARequest.Params.ContainsKey(LParam.ParamName), ARequest.Params.Value<String>(LParam.ParamName), '');
  // Transform
  if LParam.Transform <> nil then
  begin
    LTransform := TTransformInfo.Create;
    LTransform.Transform := LParam.Transform.Create as TTransformPipe;
    LTransform.Value := LValue;
    LTransform.Metadata := TTransformArguments.Create([TValue.FromVariant(LParam.Value)],
                                                      LParam.TagName,
                                                      LParam.ParamName,
                                                      LParam.Message,
                                                      nil);
    FTransforms.Add(LTransform);
  end;
  // Validation
  if LParam.Validation <> nil then
  begin
    LValidation := TValidationInfo.Create;
    LValidation.Value := TValue.Empty;
    LValidation.Validator := LParam.Validation.Create as TValidatorConstraint;
    LValidation.Args := TValidationArguments.Create([''],
                                                    LParam.TagName,
                                                    LParam.ParamName,
                                                    LParam.Message, 'param', nil);
    FValidations.Add(LValidation);
  end;
end;

procedure TValidationPipe._MapValidation(const AClass: TClass;
  const ARequest: IRouteRequest);
var
  LRttiType: TRttiType;
begin
  LRttiType := FContext.GetType(AClass);
  _ResolvePayLoads(LRttiType, ARequest);
end;

function TValidationPipe._ArrayMerge<T>(const AArray1, AArray2: TArray<T>): TArray<T>;
var
  LLength1: Integer;
  LLength2: Integer;
begin
  LLength1 := Length(AArray1);
  LLength2 := Length(AArray2);
  if (LLength1 = 0) and (LLength2 = 0) then
  begin
    Result := [];
    exit;
  end;
  SetLength(Result, LLength1 + LLength2);
  if LLength1 > 0 then
    Move(AArray1[0], Result[0], LLength1 * SizeOf(T));
  if LLength2 > 0 then
    Move(AArray2[0], Result[LLength1], LLength2 * SizeOf(T));
end;

procedure TValidationPipe._MapPipes(const AClass: TClass;
  const ARequest: IRouteRequest);
var
  LRttiType: TRttiType;
begin
  LRttiType := FContext.GetType(AClass);
  _ResolvePipes(AClass, LRttiType, ARequest);
end;

procedure TValidationPipe._ResolvePayLoads(const ARttiType: TRttiType;
  const ARequest: IRouteRequest);
var
  LMethod: TRttiMethod;
  LDecorator: TCustomAttribute;
begin
  { TODO -oIsaque -cPerformance : Implementar threads nos FORs }
  { TODO -oIsaque -cCache : Estudar uma forma de fazer cache dos decorators e
                            aqui buscar do cache e n?o fazer reflex?o }

  {$IFDEF DEBUG}
  DebugPrint('RttiType -> ' + ARttiType.Name);
  {$ENDIF}
  for LMethod in ARttiType.GetMethods do
  begin
    // LMethod.HasAttribute<>;
    // Declare your end pointers as 'published';
    // this will give you better performance in reflection.
    if LMethod.Visibility <> TMemberVisibility.mvPublished then
     Continue;

    {$IFDEF DEBUG}
    DebugPrint('Method -> ' + LMethod.Name);
    {$ENDIF}
    for LDecorator in LMethod.GetAttributes do
    begin
      {$IFDEF DEBUG}
      DebugPrint('Decorator -> ' + LDecorator.ClassName);
      {$ENDIF}
      if LDecorator is BodyAttribute then
        _ResolveBody(LDecorator, ARequest)
      else
      if LDecorator is ParamAttribute then
        _ResolveParams(LDecorator, ARequest)
      else
      if LDecorator is QueryAttribute then
        _ResolveQuerys(LDecorator, ARequest);
    end;
  end;
end;

procedure TValidationPipe._ResolvePipes(const AClass: TClass;
  const ARttiType: TRttiType; const ARequest: IRouteRequest);
var
  LProperty: TRttiProperty;
  LDecorator: TCustomAttribute;
  LValidation: IValidationInfo;
  LIsAttribute: IsAttribute;
  LValues: TList<TValue>;
  LParams_0: TArray<TValue>;
  LParams_X: TArray<TValue>;
  LClassType: TClass;
  LKey: String;
  LFor: Integer;
begin
  LClassType := nil;
  for LProperty in ARttiType.GetProperties do
  begin
    if LProperty.PropertyType.TypeKind = tkClass then
    begin
      LClassType := LProperty.GetValue(AClass).AsClass;
      // Map Object
      _MapPipes(LClassType, ARequest);
    end;
    for LDecorator in LProperty.GetAttributes do
    begin
      LIsAttribute := IsAttribute(LDecorator);
      LKey := AClass.ClassName + '->' + LProperty.Name;
      LParams_0 := IsAttribute(LDecorator).Params;
      if FJsonMapped.TryGetValue(LKey, LValues) then
      begin
        for LFor := 0 to LValues.Count -1 do
        begin
          LParams_X := _ArrayMerge<TValue>(LParams_0, [LFor]);
          LValidation := TValidationInfo.Create;
          LValidation.Value := LValues[LFor];
          LValidation.Validator := LIsAttribute.Validation.Create as TValidatorConstraint;
          LValidation.Args := TValidationArguments.Create(LParams_X,
                                                          LIsAttribute.TagName,
                                                          LProperty.Name,
                                                          LIsAttribute.Message,
                                                          AClass.ClassName,
                                                          LClassType);
          FValidations.Add(LValidation);
        end;
      end;
    end;
  end;
end;

procedure TValidationPipe._ResolveQuerys(const ADecorator: TCustomAttribute;
  const ARequest: IRouteRequest);
var
  LValue: TValue;
  LQuery: QueryAttribute;
  LTransform: ITransformInfo;
  LValidation: IValidationInfo;
begin
  LQuery := QueryAttribute(ADecorator);
  LValue := IfThen(ARequest.Querys.ContainsKey(LQuery.QueryName), ARequest.Querys.Value<String>(LQuery.QueryName), '');
  // Transform
  if LQuery.Transform <> nil then
  begin
    LTransform := TTransformInfo.Create;
    LTransform.Transform := LQuery.Transform.Create as TTransformPipe;
    LTransform.Value := LValue;
    LTransform.Metadata := TTransformArguments.Create([TValue.FromVariant(LQuery.Value)],
                                                      LQuery.TagName,
                                                      LQuery.QueryName,
                                                      LQuery.Message,
                                                      nil);
    FTransforms.Add(LTransform);
  end;
  // Validation
  if LQuery.Validation <> nil then
  begin
    LValidation := TValidationInfo.Create;
    LValidation.Value := LValue;
    LValidation.Validator := LQuery.Validation.Create as TValidatorConstraint;
    LValidation.Args := TValidationArguments.Create([''],
                                                    LQuery.TagName,
                                                    LQuery.QueryName,
                                                    LQuery.Message, 'query', nil);
    FValidations.Add(LValidation);
  end;
end;

function TValidationPipe.BuildMessages: String;
var
  LJsonArray: String;
  LJsonItem: String;
  LFor: Integer;
begin
  LJsonArray := '[';
  for LFor := 0 to FMessages.AsRef.Count - 1 do
  begin
    LJsonItem := Format('"%s"', [FMessages.AsRef[LFor]]);
    if LFor < FMessages.AsRef.Count - 1 then
      LJsonItem := LJsonItem + ',';
    LJsonArray := LJsonArray + LJsonItem;
  end;
  LJsonArray := LJsonArray + ']';
  Result := Format('{"statusCode": "400", "message": %s, "error": "Bad Request"}', [LJsonArray]);
end;

{ TValidation }

function TValidationInfo._GetValidator: IValidatorConstraint;
begin
  Result := FValidationPipe;
end;

function TValidationInfo._GetValue: TValue;
begin
  Result := FValue;
end;

function TValidationInfo._GetValidationArguments: IValidationArguments;
begin
  Result := FValidationArguments;
end;

procedure TValidationInfo._SetValidator(const Value: IValidatorConstraint);
begin
  FValidationPipe := Value;
end;

procedure TValidationInfo._SetValue(const Value: TValue);
begin
  FValue := Value;
end;

procedure TValidationInfo._SetValidationArguments(const Value: IValidationArguments);
begin
  FValidationArguments := Value;
end;

{ TTransformInfo }

function TTransformInfo._GetTransformArguments: ITransformArguments;
begin
  Result := FConvertArguments;
end;

function TTransformInfo._GetValue: TValue;
begin
  Result := FValue;
end;

function TTransformInfo._GetTransform: ITransformPipe;
begin
  Result := FConvertPipe;
end;

procedure TTransformInfo._SetTransformArguments(const Value: ITransformArguments);
begin
  FConvertArguments := Value;
end;

procedure TTransformInfo._SetValue(const Value: TValue);
begin
  FValue := Value;
end;

procedure TTransformInfo._SetTransform(const Value: ITransformPipe);
begin
  FConvertPipe := Value;
end;

end.


