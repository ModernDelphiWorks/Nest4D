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

unit Nidus.Decorator.Include;

interface

uses
  Nidus.Decorator.Param,
  Nidus.Decorator.Body,
  Nidus.Decorator.Query,
  Nidus.Decorator.IsBase,
  Nidus.Decorator.IsString,
  Nidus.Decorator.IsInteger,
  Nidus.Decorator.IsNotEmpty,
  Nidus.Decorator.IsBoolean,
  Nidus.Decorator.IsNumber,
  Nidus.Decorator.IsObject,
  Nidus.Decorator.IsArray,
  Nidus.Decorator.IsDate,
  Nidus.Decorator.IsEnum,
  Nidus.Decorator.IsEmpty,
  Nidus.Decorator.IsMax,
  Nidus.Decorator.IsMin,
  Nidus.Decorator.IsMinLength,
  Nidus.Decorator.IsMaxLength,
  Nidus.Decorator.IsAlpha,
  Nidus.Decorator.IsAlphaNumeric,
  Nidus.Decorator.Contains,
  Nidus.Decorator.IsLength;

type
  ParamAttribute = Nidus.Decorator.param.ParamAttribute;
  QueryAttribute = Nidus.Decorator.query.QueryAttribute;
  BodyAttribute = Nidus.Decorator.body.BodyAttribute;
  IsAttribute = Nidus.Decorator.isbase.IsAttribute;
  IsEmptyAttribute = Nidus.Decorator.isempty.IsEmptyAttribute;
  IsNotEmptyAttribute = Nidus.Decorator.isnotempty.IsNotEmptyAttribute;
  IsStringAttribute = Nidus.Decorator.isString.IsStringAttribute;
  IsIntegerAttribute = Nidus.Decorator.isinteger.IsIntegerAttribute;
  IsBooleanAttribute = Nidus.Decorator.isBoolean.IsBooleanAttribute;
  IsNumberAttribute = Nidus.Decorator.isnumber.IsnumberAttribute;
  IsObjectAttribute = Nidus.Decorator.isobject.IsObjectAttribute;
  IsArrayAttribute = Nidus.Decorator.isarray.IsArrayAttribute;
  IsEnumAttribute = Nidus.Decorator.isenum.IsEnumAttribute;
  IsDateAttribute = Nidus.Decorator.isdate.IsDateAttribute;
  IsMinAttribute = Nidus.Decorator.ismin.IsMinAttribute;
  IsMaxAttribute = Nidus.Decorator.ismax.IsMaxAttribute;
  IsMinLengthAttribute = Nidus.Decorator.isminlength.IsMinLengthAttribute;
  IsMaxLengthAttribute = Nidus.Decorator.ismaxlength.IsMaxLengthAttribute;
  IsLengthAttribute = Nidus.Decorator.islength.IsLengthAttribute;
  IsAlphaAttribute = Nidus.Decorator.isalpha.IsAlphaAttribute;
  IsAlphaNumericAttribute = Nidus.Decorator.isalphanumeric.IsAlphaNumericAttribute;
  ContainsAttribute = Nidus.Decorator.contains.ContainsAttribute;

implementation

end.





