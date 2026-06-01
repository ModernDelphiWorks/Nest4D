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

unit Nidus.Decorator.IsLocale;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  IsLocaleAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsLocaleAttribute }

constructor IsLocaleAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsLocale';
end;

function IsLocaleAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsLocale quando disponivel
  Result := nil;
end;

end.

