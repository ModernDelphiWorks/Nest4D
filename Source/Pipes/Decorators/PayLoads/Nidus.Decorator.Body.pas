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

unit Nidus.Decorator.Body;

interface

uses
  SysUtils,
  Variants,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types,
  Nidus.Parse.ArrayOf.Pipe;

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



