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

unit Nidus.Validator.Constraint;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  Nidus.Validation.Interfaces;

type
  TValidatorConstraint = class(TInterfacedObject, IValidatorConstraint)
  public
    function Validate(const Value: TValue;
      const Args: IValidationArguments): TResultValidation; virtual; abstract;
  end;

implementation

end.






