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

unit Nidus.Decorator.ArrayContains;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types,
  Nidus.Validation.ArrayContains;

type
  ArrayContainsAttribute = class(IsAttribute)
  private
    FValue: TArray<TValue>;
  public
    constructor Create(const AValue: TArray<TValue>; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ ArrayContains }

constructor ArrayContainsAttribute.Create(const AValue: TArray<TValue>;
  const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'ArrayContains';
  FValue := AValue;
end;

function ArrayContainsAttribute.Params: TArray<TValue>;
begin
  Result := FValue;
end;

function ArrayContainsAttribute.Validation: TValidation;
begin
  Result := TArrayContains;
end;

end.





