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

unit Nidus.Validation.IsNumber;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  Nidus.Validator.Constraint,
  Nidus.Validation.Interfaces;

type
  TIsNumber = class(TValidatorConstraint)
  public
    function Validate(const Value: TValue;
      const Args: IValidationArguments): TResultValidation; override;
  end;

implementation

{ TIsNumber }

function TIsNumber.Validate(const Value: TValue;
  const Args: IValidationArguments): TResultValidation;
var
  LMessage: String;
begin
  Result.Success(False);
  if Value.IsType<Double> or Value.IsType<Single> or Value.IsType<Extended> then
    Result.Success(True);
  if not Result.ValueSuccess then
  begin
    LMessage := IfThen(Args.Message = '',
                       Format('[%s] %s->%s [%s] must be a number conforming to the specified constraints',
                       [Args.TagName,
                        Args.TypeName,
                        Args.Values[Length(Args.Values) -1].ToString,
                        Args.FieldName]), Args.Message);
    Result.Failure(LMessage);
  end;
end;

end.






