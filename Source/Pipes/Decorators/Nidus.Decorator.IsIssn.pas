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

unit Nidus.Decorator.IsIssn;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  IsISSNAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsISSNAttribute }

constructor IsISSNAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsISSN';
end;

function IsISSNAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao IsISSN quando disponivel
  Result := nil;
end;

end.

