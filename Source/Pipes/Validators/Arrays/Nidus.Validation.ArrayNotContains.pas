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

unit Nidus.Validation.ArrayNotContains;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  Nidus.Validator.Constraint,
  Nidus.Validation.Interfaces;

type
  TArrayNotContains = class(TValidatorConstraint)
  private
    function _ArrayToString(const AValues: TArray<TValue>): String;
    function _ArrayNotContainsAllElements(const ASource, AValues: TArray<TValue>): Boolean;
  public
    function Validate(const Value: TValue;
      const Args: IValidationArguments): TResultValidation; override;
  end;

implementation

{ TArrayContains }

function TArrayNotContains.Validate(const Value: TValue;
  const Args: IValidationArguments): TResultValidation;
var
  LMessage: String;
begin
  Result.Success(False);
  if Value.Kind in [tkArray, tkDynArray] then
  begin
    if _ArrayNotContainsAllElements(Value.AsType<TArray<TValue>>, Args.Values) then
      Result.Success(True);
  end;
  if not Result.ValueSuccess then
  begin
    LMessage := IfThen(Args.Message = '',
                       Format('[%s] %s->%s [%s] must contain a %s values',
                       [Args.TagName,
                        Args.TypeName,
                        Args.Values[Length(Args.Values) -1].ToString,
                        Args.FieldName,
                        _ArrayToString(Args.Values)]), Args.Message);
    Result.Failure(LMessage);
  end;
end;

function TArrayNotContains._ArrayToString(const AValues: TArray<TValue>): String;
var
  LItem: TValue;
begin
  Result := '';
  for LItem in AValues do
    Result := Result + LItem.ToString + ', ';
end;

function TArrayNotContains._ArrayNotContainsAllElements(const ASource, AValues: TArray<TValue>): Boolean;
var
  LFor, LFind: integer;
  LFound: Boolean;
begin
  Result := True;
  for LFor := Low(AValues) to High(AValues) do
  begin
    LFound := False;
    for LFind := Low(ASource) to High(ASource) do
    begin
      if AValues[LFor].ToString <> ASource[LFind].ToString then
      begin
        LFound := True;
        break;
      end;
    end;
    if not LFound then
    begin
      Result := False;
      exit;
    end;
  end;
end;

end.





