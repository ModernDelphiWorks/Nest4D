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

unit Nidus.Validation.IsPositive;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  Nidus.Validator.Constraint,
  Nidus.Validation.Interfaces;

type
  TIspositive = class(TValidatorConstraint)
  public
    function Validate(const Value: TValue;
      const Args: IValidationArguments): TResultValidation; override;
  end;

implementation

{ TIspositive }

function TIspositive.Validate(const Value: TValue;
  const Args: IValidationArguments): TResultValidation;
var
  LMessage: String;
begin
  Result.Success(False);
  
  // TODO: Implement validation logic for ispositive
  // This is a template - implement the actual validation logic
  
  if not Result.ValueSuccess then
  begin
    LMessage := IfThen(Args.Message = '',
                       Format('[%s] %s->%s [%s] validation failed for ispositive',
                       [Args.TagName,
                        Args.TypeName,
                        Args.Values[Length(Args.Values) -1].ToString,
                        Args.FieldName]), Args.Message);
    Result.Failure(LMessage);
  end;
end;

end.
