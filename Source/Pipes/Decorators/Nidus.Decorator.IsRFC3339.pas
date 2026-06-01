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

unit Nidus.Decorator.IsRFC3339;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  IsRFC3339Attribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsRFC3339Attribute }

constructor IsRFC3339Attribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsRFC3339';
end;

function IsRFC3339Attribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsRFC3339 quando disponivel
  Result := nil;
end;

end.

