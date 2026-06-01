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

unit Nidus.Decorator.IsISO31661Alpha3;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  IsISO31661Alpha3Attribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsISO31661Alpha3Attribute }

constructor IsISO31661Alpha3Attribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsISO31661Alpha3';
end;

function IsISO31661Alpha3Attribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsISO31661Alpha3 quando disponivel
  Result := nil;
end;

end.

