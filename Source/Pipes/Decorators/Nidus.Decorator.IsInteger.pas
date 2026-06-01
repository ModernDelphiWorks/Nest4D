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

unit Nidus.Decorator.IsInteger;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types,
  Nidus.Validation.IsInteger;

type
  IsIntegerAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsStringAttribute }

constructor IsIntegerAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsInteger';
end;

function IsIntegerAttribute.Validation: TValidation;
begin
  Result := TIsInteger;
end;

end.







