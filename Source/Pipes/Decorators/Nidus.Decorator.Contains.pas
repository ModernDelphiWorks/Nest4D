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

unit Nidus.Decorator.Contains;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types,
  Nidus.Validation.Contains;

type
  ContainsAttribute = class(IsAttribute)
  private
    FValue: String;
  public
    constructor Create(const AValue: String; const AMessage: String = '');
      reintroduce;
    function validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsArrayAttribute }

constructor ContainsAttribute.Create(const AValue: String;
  const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Contains';
  FValue := AValue;
end;

function ContainsAttribute.Params: TArray<TValue>;
begin
  Result := [FValue];
end;

function ContainsAttribute.validation: TValidation;
begin
  Result := TContains;
end;

end.


