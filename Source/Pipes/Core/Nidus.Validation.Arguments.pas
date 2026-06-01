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

unit Nidus.Validation.Arguments;

interface

uses
  Rtti,
  Nidus.Validation.Interfaces;

type
  TValidationArguments = class(TInterfacedObject, IValidationArguments)
  private
    FValues: TArray<TValue>;
    FObjectType: TClass;
    FTagName: String;
    FFieldName: String;
    FMessage: String;
    FTypeName: String;
  public
    constructor Create(const AValues: TArray<TValue>;
      const ATagName: String; const AFieldName: String; const AMessage: String;
      const ATypeName: String; const AObjectType: TClass);
    function Values: TArray<TValue>;
    function TagName: String;
    function FieldName: String;
    function Message: String;
    function TypeName: String;
    function ObjectType: TClass;
  end;

implementation

{ TArgumentMetadata }

function TValidationArguments.ObjectType: TClass;
begin
  Result := FObjectType;
end;

constructor TValidationArguments.Create(const AValues: TArray<TValue>;
  const ATagName: String; const AFieldName: String;
  const AMessage: String; const ATypeName: String; const AObjectType: TClass);
begin
  FTagName := ATagName;
  FFieldName := AFieldName;
  FValues := AValues;
  FMessage := AMessage;
  FTypeName := ATypeName;
  FObjectType := AObjectType;
end;

function TValidationArguments.FieldName: String;
begin
  Result := FFieldName;
end;

function TValidationArguments.Message: String;
begin
  Result := FMessage;
end;

function TValidationArguments.TagName: String;
begin
  Result := FTagName;
end;

function TValidationArguments.TypeName: String;
begin
  Result := FTypeName;
end;

function TValidationArguments.Values: TArray<TValue>;
begin
  Result := FValues;
end;

end.





