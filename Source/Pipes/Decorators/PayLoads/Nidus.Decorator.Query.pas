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

unit Nidus.Decorator.Query;

interface

uses
  SysUtils,
  Variants,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

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
  Nidus.Transform.Pipe;

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




