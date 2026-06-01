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

unit Nidus.Validation.IsMinLength;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  Nidus.Validator.Constraint,
  Nidus.Validation.Interfaces;

type
  TIsMinLength = class(TValidatorConstraint)
  public
    function Validate(const Value: TValue;
      const Args: IValidationArguments): TResultValidation; override;
  end;

implementation

{ TIsMax }

function TIsMinLength.Validate(const Value: TValue;
  const Args: IValidationArguments): TResultValidation;
var
  LMessage: String;
  LLength: integer;
begin
  Result.Success(False);
  if Value.Kind in [tkString, tkLString, tkWString, tkUString] then
  begin
    LLength := Length(Value.AsString);
    // Args.Values[0]=Min
    if Args.Values[0].Kind in [tkInt64, tkInteger, tkFloat] then
    begin
      if LLength >= Args.Values[0].AsExtended then
        Result.Success(True);
    end;
  end;
  if not Result.ValueSuccess then
  begin
    LMessage := IfThen(Args.Message = '',
                       Format('[%s] %s->%s [%s] must be longer than or equal to %s characters',
                       [Args.TagName,
                        Args.TypeName,
                        Args.Values[Length(Args.Values) -1].ToString,
                        Args.FieldName,
                        Args.Values[0].ToString]), Args.Message);
    Result.Failure(LMessage);
  end;
end;

end.





