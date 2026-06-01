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

unit Nidus.Transform.Arguments;

interface

uses
  Rtti,
  Nidus.Transform.Interfaces;

type
  TTransformArguments = class(TInterfacedObject, ITransformArguments)
  private
    FValues: TArray<TValue>;
    FTagName: String;
    FFieldName: String;
    FObjectType: TClass;
    FMessage: String;
  public
    constructor Create(const AValues: TArray<TValue>;
      const ATagName: String; const AFieldName: String;
      const AMessage: String; const AObjectType: TClass);
    function TagName: String;
    function FieldName: String;
    function Values: TArray<TValue>;
    function Message: String;
    function ObjectType: TClass;
  end;

implementation

{ TConverterArguments }

constructor TTransformArguments.Create(const AValues: TArray<TValue>;
  const ATagName: String; const AFieldName: String;
  const AMessage: String; const AObjectType: TClass);
begin
  FTagName := ATagName;
  FFieldName := AFieldName;
  FValues := AValues;
  FMessage := AMessage;
  FObjectType := AObjectType;
end;

function TTransformArguments.FieldName: String;
begin
  Result := FFieldName;
end;

function TTransformArguments.Message: String;
begin
  Result := FMessage;
end;

function TTransformArguments.ObjectType: TClass;
begin
  Result := FObjectType;
end;

function TTransformArguments.TagName: String;
begin
  Result := FTagName;
end;

function TTransformArguments.Values: TArray<TValue>;
begin
  Result := FValues;
end;

end.





