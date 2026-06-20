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

unit Nidus.Validation.IsStrongPassword;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  Nidus.Validator.Constraint,
  Nidus.Validation.Interfaces;

type
  TIsstrongpassword = class(TValidatorConstraint)
  public
    function Validate(const Value: TValue;
      const Args: IValidationArguments): TResultValidation; override;
  end;

implementation

{ TIsstrongpassword }

function TIsstrongpassword.Validate(const Value: TValue;
  const Args: IValidationArguments): TResultValidation;
const
  CMinLength = 8;
var
  LMessage, LPwd: string;
  LCh: Char;
  LHasUpper, LHasLower, LHasDigit, LHasSpecial: Boolean;
begin
  Result.Success(False);
  
  if Value.Kind in [tkString, tkLString, tkWString, tkUString] then
  begin
    LPwd := Value.ToString;
    LHasUpper := False;
    LHasLower := False;
    LHasDigit := False;
    LHasSpecial := False;
    for LCh in LPwd do
      case LCh of
        'A'..'Z': LHasUpper := True;
        'a'..'z': LHasLower := True;
        '0'..'9': LHasDigit := True;
      else
        if not CharInSet(LCh, [#0..#32]) then
          LHasSpecial := True;
      end;
    if (Length(LPwd) >= CMinLength) and LHasUpper and LHasLower and
       LHasDigit and LHasSpecial then
      Result.Success(True);
  end;
  
  if not Result.ValueSuccess then
  begin
    LMessage := IfThen(Args.Message = '',
                       Format('[%s] %s->%s [%s] must be a strong password ' +
                       '(min %d chars with upper, lower, digit and special)',
                       [Args.TagName,
                        Args.TypeName,
                        Args.Values[Length(Args.Values) -1].ToString,
                        Args.FieldName,
                        CMinLength]), Args.Message);
    Result.Failure(LMessage);
  end;
end;

end.
