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

unit Nidus.Decorator.IsISO8601;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  IsISO8601Attribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsISO8601Attribute }

constructor IsISO8601Attribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsISO8601';
end;

function IsISO8601Attribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsISO8601 quando disponivel
  Result := nil;
end;

end.

