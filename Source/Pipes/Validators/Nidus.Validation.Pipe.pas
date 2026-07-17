{
  ------------------------------------------------------------------------------
  Nidus
  Modular and scalable application framework for Delphi, inspired by the architectural patterns of NestJS.

  SPDX-License-Identifier: MIT
  Copyright (c) 2025-2026 Isaque Pinheiro

  Licensed under the MIT License.
  See the LICENSE file in the project root for full license information.
  ------------------------------------------------------------------------------
}

unit Nidus.Validation.Pipe;

interface

uses
  Rtti,
  TypInfo,
  Classes,
  SyncObjs,
  SysUtils,
  StrUtils,
  Generics.Collections,
  ModernSyntax.ResultPair,
  ModernSyntax.Objects,
  ModernSyntax.Std,
  Nidus.Route.Handler,
  Nidus.Decorator.Include,
  Nidus.Validation.Include,
  Nidus.Validation.Interfaces,
  Nidus.Transform.Interfaces,
  Nidus.Request;

type
  TValidations = class(TList<IValidationInfo>);
  TTransforms = class(TList<ITransformInfo>);

  // THREAD-SAFETY (re-entrancy): the per-request working state that Validate used to
  // keep in instance fields is gathered here and lives as a LOCAL of Validate, passed
  // by pointer to the private helpers. TValidationPipe is a process-wide singleton
  // (registered once via GetNidus.UsePipes(TValidationPipe.Create)); keeping this state
  // on the instance made concurrent requests share+free the same dictionaries ->
  // cross-thread use-after-free / double-free (FastMM "virtual method on a freed
  // object" / "FreeMem block header corrupted"). As locals, each concurrent Validate is
  // self-contained.
  PValidationWork = ^TValidationWork;
  TValidationWork = record
    Context: TRttiContext;
    Validations: TValidations;
    Transforms: TTransforms;
    JsonMapped: TJsonMapped;
    Messages: TList<String>;   // the CURRENT thread's message list (owned by FThreadMessages)
  end;

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
    // Per-thread validation messages (mirrors the InjectContainer#2 per-thread
    // partition): the Validate->IsMessages->BuildMessages contract runs on one thread,
    // so each thread keeps its own reusable message list. FLock guards ONLY the
    // dictionary get/create; the returned list is touched by its owning thread.
    FThreadMessages: TObjectDictionary<TThreadID, TList<String>>;
    FLock: TCriticalSection;
    function _CurrentMessages: TList<String>;
    procedure _MapPipes(const AWork: PValidationWork; const AClass: TClass; const ARequest: IRouteRequest); inline;
    procedure _MapValidation(const AWork: PValidationWork; const AClass: TClass; const ARequest: IRouteRequest); inline;
    procedure _ResolveParams(const AWork: PValidationWork; const ADecorator: TCustomAttribute; const ARequest: IRouteRequest); inline;
    procedure _ResolveQuerys(const AWork: PValidationWork; const ADecorator: TCustomAttribute; const ARequest: IRouteRequest); inline;
    procedure _ResolvePipes(const AWork: PValidationWork; const AClass: TClass; const ARttiType: TRttiType;
      const ARequest: IRouteRequest); inline;
    procedure _ResolvePayLoads(const AWork: PValidationWork; const ARttiType: TRttiType;
      const ARequest: IRouteRequest); inline;
    procedure _ResolveBody(const AWork: PValidationWork; const ADecorator: TCustomAttribute; const ARequest: IRouteRequest);
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
  FThreadMessages := TObjectDictionary<TThreadID, TList<String>>.Create([doOwnsValues]);
  FLock := TCriticalSection.Create;
end;

destructor TValidationPipe.Destroy;
begin
  FThreadMessages.Free;   // doOwnsValues frees every per-thread list
  FLock.Free;
  inherited;
end;

function TValidationPipe._CurrentMessages: TList<String>;
begin
  // Short lock around the dict get/create only; the list is then used lock-free by
  // its owning thread. Reused across requests on the same (pooled) Horse worker.
  FLock.Enter;
  try
    if not FThreadMessages.TryGetValue(TThread.CurrentThread.ThreadID, Result) then
    begin
      Result := TList<String>.Create;
      FThreadMessages.Add(TThread.CurrentThread.ThreadID, Result);
    end;
  finally
    FLock.Leave;
  end;
end;

function TValidationPipe.IsMessages: Boolean;
var
  LMessages: TList<String>;
begin
  Result := False;
  FLock.Enter;
  try
    if FThreadMessages.TryGetValue(TThread.CurrentThread.ThreadID, LMessages) then
      Result := LMessages.Count > 0;
  finally
    FLock.Leave;
  end;
end;

procedure TValidationPipe.Validate(const AClass: TClass;
  const ARequest: IRouteRequest);
var
  LWork: TValidationWork;
  LMessages: TList<String>;
  LValidator: IValidationInfo;
  LInfo: ITransformInfo;
  LResultTransform: TResultTransform;
  LResultValidation: TResultValidation;
begin
  // Per-request working state is LOCAL (re-entrant): no shared instance fields.
  LMessages := _CurrentMessages;
  LMessages.Clear;
  LWork.Context := TRttiContext.Create;
  LWork.Validations := TValidations.Create;
  LWork.Transforms := TTransforms.Create;
  LWork.JsonMapped := TJsonMapped.Create([doOwnsValues]);
  LWork.Messages := LMessages;
  { TODO -oIsaque -cPerformance : Implementar threads nos FORs }
  try
    _MapValidation(@LWork, AClass, ARequest);
    // Transforms
    for LInfo in LWork.Transforms do
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
          LMessages.Add(Msg);
        end);
    end;
    // Validations
    for LValidator in LWork.Validations do
    begin
      LResultValidation := LValidator.Validator.Validate(LValidator.Value,
                                                         LValidator.Args);
      LResultValidation.When(
        procedure(Value: Boolean)
        begin

        end,
        procedure(Msg: String)
        begin
          LMessages.Add(Msg);
        end);
    end;
  finally
    LWork.JsonMapped.Free;
    LWork.Validations.Free;
    LWork.Transforms.Free;
    LWork.Context.Free;
  end;
end;

procedure TValidationPipe._ResolveBody(const AWork: PValidationWork;
  const ADecorator: TCustomAttribute; const ARequest: IRouteRequest);
var
  LBody: BodyAttribute;
  LTransform: ITransformInfo;
  LValue: TValue;
  LResultBody: TResultTransform;
  LObject: IModernObject;
  LJsonMapped: TJsonMapped;
  LMessages: TList<String>;
begin
  // Copy to locals so the anonymous methods below capture the per-request state, not
  // an instance field (there are no fields anymore).
  LJsonMapped := AWork.JsonMapped;
  LMessages := AWork.Messages;
  LBody := BodyAttribute(ADecorator);
  LValue := ARequest.Body;
  if LBody.Transform <> nil then
  begin
    if LBody.Transform.InheritsFrom(TParseJsonPipe) then
    begin
      LObject := TModernObject.New;
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
            LJsonMapped.AddOrSetValue(LItem.Key, TList<TValue>.Create(LItem.Value));
        end,
        procedure(Msg: String)
        begin
          LMessages.Add(Msg);
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
        AWork.Transforms.Add(LTransform);
      end;
    end;
  end;
  _MapPipes(AWork, LBody.ObjectType, ARequest);
end;

procedure TValidationPipe._ResolveParams(const AWork: PValidationWork;
  const ADecorator: TCustomAttribute; const ARequest: IRouteRequest);
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
    AWork.Transforms.Add(LTransform);
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
    AWork.Validations.Add(LValidation);
  end;
end;

procedure TValidationPipe._MapValidation(const AWork: PValidationWork;
  const AClass: TClass; const ARequest: IRouteRequest);
var
  LRttiType: TRttiType;
begin
  LRttiType := AWork.Context.GetType(AClass);
  _ResolvePayLoads(AWork, LRttiType, ARequest);
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

procedure TValidationPipe._MapPipes(const AWork: PValidationWork;
  const AClass: TClass; const ARequest: IRouteRequest);
var
  LRttiType: TRttiType;
begin
  LRttiType := AWork.Context.GetType(AClass);
  _ResolvePipes(AWork, AClass, LRttiType, ARequest);
end;

procedure TValidationPipe._ResolvePayLoads(const AWork: PValidationWork;
  const ARttiType: TRttiType; const ARequest: IRouteRequest);
var
  LMethod: TRttiMethod;
  LDecorator: TCustomAttribute;
begin
  { TODO -oIsaque -cPerformance : Implementar threads nos FORs }
  { TODO -oIsaque -cCache : Estudar uma forma de fazer cache dos decorators e
                            aqui buscar do cache e n?o fazer reflex?o }

  {$IFDEF DEBUG_NIDUS_VALIDATION_PIPE}
  DebugPrint('RttiType -> ' + ARttiType.Name);
  {$ENDIF}
  for LMethod in ARttiType.GetMethods do
  begin
    // LMethod.HasAttribute<>;
    // Declare your end pointers as 'published';
    // this will give you better performance in reflection.
    if LMethod.Visibility <> TMemberVisibility.mvPublished then
     Continue;

    {$IFDEF DEBUG_NIDUS_VALIDATION_PIPE}
    DebugPrint('Method -> ' + LMethod.Name);
    {$ENDIF}
    for LDecorator in LMethod.GetAttributes do
    begin
      {$IFDEF DEBUG_NIDUS_VALIDATION_PIPE}
      DebugPrint('Decorator -> ' + LDecorator.ClassName);
      {$ENDIF}
      if LDecorator is BodyAttribute then
        _ResolveBody(AWork, LDecorator, ARequest)
      else
      if LDecorator is ParamAttribute then
        _ResolveParams(AWork, LDecorator, ARequest)
      else
      if LDecorator is QueryAttribute then
        _ResolveQuerys(AWork, LDecorator, ARequest);
    end;
  end;
end;

procedure TValidationPipe._ResolvePipes(const AWork: PValidationWork;
  const AClass: TClass; const ARttiType: TRttiType; const ARequest: IRouteRequest);
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
      _MapPipes(AWork, LClassType, ARequest);
    end;
    for LDecorator in LProperty.GetAttributes do
    begin
      LIsAttribute := IsAttribute(LDecorator);
      LKey := AClass.ClassName + '->' + LProperty.Name;
      LParams_0 := IsAttribute(LDecorator).Params;
      if AWork.JsonMapped.TryGetValue(LKey, LValues) then
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
          AWork.Validations.Add(LValidation);
        end;
      end;
    end;
  end;
end;

procedure TValidationPipe._ResolveQuerys(const AWork: PValidationWork;
  const ADecorator: TCustomAttribute; const ARequest: IRouteRequest);
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
    AWork.Transforms.Add(LTransform);
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
    AWork.Validations.Add(LValidation);
  end;
end;

function TValidationPipe.BuildMessages: String;
var
  LMessages: TList<String>;
  LJsonArray: String;
  LJsonItem: String;
  LFor: Integer;
begin
  LMessages := _CurrentMessages;
  LJsonArray := '[';
  for LFor := 0 to LMessages.Count - 1 do
  begin
    LJsonItem := Format('"%s"', [LMessages[LFor]]);
    if LFor < LMessages.Count - 1 then
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
