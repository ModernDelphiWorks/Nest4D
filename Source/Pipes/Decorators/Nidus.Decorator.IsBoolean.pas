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

unit Nidus.Decorator.IsBoolean;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types,
  Nidus.Validation.IsBoolean;

type
  IsbooleanAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsbooleanAttribute }

constructor IsbooleanAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Isboolean';
end;

function IsbooleanAttribute.Validation: TValidation;
begin
  Result := TIsBoolean;
end;

end.







