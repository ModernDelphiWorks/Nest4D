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

unit Nidus.Validation.Include;

interface

uses
  Rtti,
  Generics.Collections,
  Nidus.Transform.Pipe,
  Nidus.Transform.Arguments,
  Nidus.Transform.Interfaces,
  Nidus.Parse.Json.Pipe,
  Nidus.Parse.Integer.Pipe,
  Nidus.Validation.Arguments,
  Nidus.Validator.Constraint,
  Nidus.Validation.IsString,
  Nidus.Validation.IsInteger,
  Nidus.Validation.IsEmpty,
  Nidus.Validation.IsNotEmpty,
  Nidus.Validation.IsArray,
  Nidus.Validation.IsObject,
  Nidus.Validation.IsNumber,
  Nidus.Validation.IsDate,
  Nidus.Validation.IsBoolean,
  Nidus.Validation.IsEnum,
  Nidus.Validation.Interfaces;

type
  TResultValidation = Nidus.Validation.interfaces.TResultValidation;
  TResultTransform = Nidus.transform.interfaces.TResultTransform;
  TJsonMapped = Nidus.transform.interfaces.TJsonMapped;
  //
  TTransformPipe = Nidus.transform.pipe.TTransformPipe;
  ITransformArguments = Nidus.transform.interfaces.ITransformArguments;
  TTransformArguments = Nidus.transform.arguments.TTransformArguments;
  //
  IValidationArguments = Nidus.Validation.interfaces.IValidationArguments;
  IValidatorConstraint = Nidus.Validation.interfaces.IValidatorConstraint;
  IValidationInfo = Nidus.Validation.interfaces.IValidationInfo;
  IValidationPipe = Nidus.Validation.interfaces.IValidationPipe;
  ITransformInfo = Nidus.transform.interfaces.ITransformInfo;
  ITransformPipe = Nidus.transform.interfaces.ITransformPipe;
  //
  TValidationArguments = Nidus.Validation.arguments.TValidationArguments;
  TValidatorConstraint = Nidus.validator.constraint.TValidatorConstraint;
  //
  TParseJsonPipe = Nidus.parse.json.pipe.TParseJsonPipe;
  TParseIntegerPipe = Nidus.parse.integer.pipe.TParseIntegerPipe;
  //
  TIsEmpty = Nidus.Validation.isempty.TIsEmpty;
  TIsNotEmpty = Nidus.Validation.isnotempty.TIsNotEmpty;
  TIsString = Nidus.Validation.isString.TIsString;
  TIsInteger = Nidus.Validation.isinteger.TIsInteger;
  TIsNumber = Nidus.Validation.isnumber.TIsNumber;
  TIsBoolean = Nidus.Validation.isBoolean.TIsBoolean;
  TIsDate = Nidus.Validation.isdate.TIsDate;
  TIsEnum = Nidus.Validation.isenum.TIsEnum;
  TIsObject = Nidus.Validation.isobject.TIsObject;
  TIsArray = Nidus.Validation.isarray.TIsArray;

implementation

end.







