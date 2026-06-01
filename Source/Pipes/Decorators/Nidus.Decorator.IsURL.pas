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

unit Nidus.Decorator.IsURL;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  IsURLAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsURLAttribute }

constructor IsURLAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsURL';
end;

function IsURLAttribute.Validation: TValidation;
begin
  // TODO: Implementar validação de URL quando disponível
  Result := nil;
end;

end.



