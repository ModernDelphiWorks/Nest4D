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

unit Nidus.Validation.Contains;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  Nidus.Validator.Constraint,
  Nidus.Validation.Interfaces;

type
  TContains = class(TValidatorConstraint)
  public
    function Validate(const Value: TValue;
      const Args: IValidationArguments): TResultValidation; override;
  end;

implementation

{ TContains }

function TContains.Validate(const Value: TValue;
  const Args: IValidationArguments): TResultValidation;
var
  LMessage: String;
begin
  Result.Success(False);
  if Value.Kind in [tkString, tkLString, tkWString, tkUString] then
  begin
    if Pos(Value.ToString, Args.Values[0].ToString) > 0 then
      Result.Success(True);
  end;
  if not Result.ValueSuccess then
  begin
    LMessage := IfThen(Args.Message = '',
                       Format('[%s] %s->%s [%s] must contain a %s String',
                       [Args.TagName,
                        Args.TypeName,
                        Args.Values[Length(Args.Values) -1].ToString,
                        Args.FieldName,
                        Args.Values[0].ToString]), Args.Message);
    Result.Failure(LMessage);
  end;
end;

end.






