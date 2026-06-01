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

unit Nidus.Decorator.IsArrayNotEmpty;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  IsArrayNotEmptyAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

constructor IsArrayNotEmptyAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
end;

function IsArrayNotEmptyAttribute.Validation: TValidation;
begin
  // TODO: Implement IsArrayNotEmpty validation logic
//  Result := TValidation.Create;
end;

end.
