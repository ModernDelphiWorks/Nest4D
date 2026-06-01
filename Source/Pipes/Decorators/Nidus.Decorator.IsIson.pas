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

unit Nidus.Decorator.IsIson;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  isisonAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ isisonAttribute }

constructor isisonAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'isison';
end;

function isisonAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao isison quando disponivel
  Result := nil;
end;

end.

