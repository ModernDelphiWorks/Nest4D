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

unit Nidus.Decorator.IsDivisibleBy;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  IsDivisibleByAttribute = class(IsAttribute)
  private
    FValue: TValue;
  public
    constructor Create(const AValue: Extended; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsDivisibleByAttribute }

constructor IsDivisibleByAttribute.Create(const AValue: Extended; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsDivisibleBy';
  FValue := AValue;
end;

function IsDivisibleByAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação IsDivisibleBy quando disponível
  Result := nil;
end;

function IsDivisibleByAttribute.Params: TArray<TValue>;
begin
  Result := [FValue];
end;

end.



