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

unit Nidus.Decorator.ArrayNotContains;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types,
  Nidus.Validation.ArrayNotContains;

type
  ArrayNotContainsAttribute = class(IsAttribute)
  private
    FValue: TArray<TValue>;
  public
    constructor Create(const AValue: TArray<TValue>;
      const AMessage: String = ''); reintroduce;
    function validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ ArrayContains }

constructor ArrayNotContainsAttribute.Create(const AValue: TArray<TValue>;
  const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'ArrayNotContains';
  FValue := AValue;
end;

function ArrayNotContainsAttribute.Params: TArray<TValue>;
begin
  Result := FValue;
end;

function ArrayNotContainsAttribute.validation: TValidation;
begin
  Result := TArrayNotContains;
end;

end.


