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

unit Nidus.Decorator.IsNotContains;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  IsNotContainsAttribute = class(IsAttribute)
  private
    FValue: TValue;
  public
    constructor Create(const AValue: String; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsNotContainsAttribute }

constructor IsNotContainsAttribute.Create(const AValue: String; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsNotContains';
  FValue := AValue;
end;

function IsNotContainsAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsNotContains quando disponível
  Result := nil;
end;

function IsNotContainsAttribute.Params: TArray<TValue>;
begin
  Result := [FValue];
end;

end.



