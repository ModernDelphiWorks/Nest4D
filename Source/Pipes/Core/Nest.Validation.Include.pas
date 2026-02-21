{
             Nest4D - Development Framework for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers?o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos ? permitido copiar e distribuir c?pias deste documento de
       licen?a, mas mud?-lo n?o ? permitido.

       Esta vers?o da GNU Lesser General Public License incorpora
       os termos e condi??es da vers?o 3 da GNU General Public License
       Licen?a, complementado pelas permiss?es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework for Delphi)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://nest4d-en.docs-br.com)
}

unit Nest.Validation.Include;

interface

uses
  Rtti,
  Generics.Collections,
  Nest.transform.pipe,
  Nest.transform.arguments,
  Nest.transform.interfaces,
  Nest.parse.json.pipe,
  Nest.parse.integer.pipe,
  Nest.Validation.arguments,
  Nest.validator.constraint,
  Nest.Validation.IsString,
  Nest.Validation.IsInteger,
  Nest.Validation.IsEmpty,
  Nest.Validation.IsNotEmpty,
  Nest.Validation.IsArray,
  Nest.Validation.IsObject,
  Nest.Validation.IsNumber,
  Nest.Validation.IsDate,
  Nest.Validation.IsBoolean,
  Nest.Validation.IsEnum,
  Nest.Validation.Interfaces;

type
  TResultValidation = Nest.Validation.interfaces.TResultValidation;
  TResultTransform = Nest.transform.interfaces.TResultTransform;
  TJsonMapped = Nest.transform.interfaces.TJsonMapped;
  //
  TTransformPipe = Nest.transform.pipe.TTransformPipe;
  ITransformArguments = Nest.transform.interfaces.ITransformArguments;
  TTransformArguments = Nest.transform.arguments.TTransformArguments;
  //
  IValidationArguments = Nest.Validation.interfaces.IValidationArguments;
  IValidatorConstraint = Nest.Validation.interfaces.IValidatorConstraint;
  IValidationInfo = Nest.Validation.interfaces.IValidationInfo;
  IValidationPipe = Nest.Validation.interfaces.IValidationPipe;
  ITransformInfo = Nest.transform.interfaces.ITransformInfo;
  ITransformPipe = Nest.transform.interfaces.ITransformPipe;
  //
  TValidationArguments = Nest.Validation.arguments.TValidationArguments;
  TValidatorConstraint = Nest.validator.constraint.TValidatorConstraint;
  //
  TParseJsonPipe = Nest.parse.json.pipe.TParseJsonPipe;
  TParseIntegerPipe = Nest.parse.integer.pipe.TParseIntegerPipe;
  //
  TIsEmpty = Nest.Validation.isempty.TIsEmpty;
  TIsNotEmpty = Nest.Validation.isnotempty.TIsNotEmpty;
  TIsString = Nest.Validation.isString.TIsString;
  TIsInteger = Nest.Validation.isinteger.TIsInteger;
  TIsNumber = Nest.Validation.isnumber.TIsNumber;
  TIsBoolean = Nest.Validation.isBoolean.TIsBoolean;
  TIsDate = Nest.Validation.isdate.TIsDate;
  TIsEnum = Nest.Validation.isenum.TIsEnum;
  TIsObject = Nest.Validation.isobject.TIsObject;
  TIsArray = Nest.Validation.isarray.TIsArray;

implementation

end.







