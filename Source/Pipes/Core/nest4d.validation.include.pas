{
             Nest4D - Development Framework for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework for Delphi)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://nest4d-en.docs-br.com)
}

unit nest4d.validation.include;

interface

uses
  Rtti,
  Generics.Collections,
  nest4d.transform.pipe,
  nest4d.transform.arguments,
  nest4d.transform.interfaces,
  nest4d.parse.json.pipe,
  nest4d.parse.integer.pipe,
  nest4d.validation.arguments,
  nest4d.validator.constraint,
  validation.isString,
  validation.isinteger,
  validation.isempty,
  validation.isnotempty,
  validation.isarray,
  validation.isobject,
  validation.isnumber,
  validation.isdate,
  validation.isBoolean,
  validation.isenum,
  nest4d.validation.interfaces;

type
  TResultValidation = nest4d.validation.interfaces.TResultValidation;
  TResultTransform = nest4d.transform.interfaces.TResultTransform;
  TJsonMapped = nest4d.transform.interfaces.TJsonMapped;
  //
  TTransformPipe = nest4d.transform.pipe.TTransformPipe;
  ITransformArguments = nest4d.transform.interfaces.ITransformArguments;
  TTransformArguments = nest4d.transform.arguments.TTransformArguments;
  //
  IValidationArguments = nest4d.validation.interfaces.IValidationArguments;
  IValidatorConstraint = nest4d.validation.interfaces.IValidatorConstraint;
  IValidationInfo = nest4d.validation.interfaces.IValidationInfo;
  IValidationPipe = nest4d.validation.interfaces.IValidationPipe;
  ITransformInfo = nest4d.transform.interfaces.ITransformInfo;
  ITransformPipe = nest4d.transform.interfaces.ITransformPipe;
  //
  TValidationArguments = nest4d.validation.arguments.TValidationArguments;
  TValidatorConstraint = nest4d.validator.constraint.TValidatorConstraint;
  //
  TParseJsonPipe = nest4d.parse.json.pipe.TParseJsonPipe;
  TParseIntegerPipe = nest4d.parse.integer.pipe.TParseIntegerPipe;
  //
  TIsEmpty = validation.isempty.TIsEmpty;
  TIsNotEmpty = validation.isnotempty.TIsNotEmpty;
  TIsString = validation.isString.TIsString;
  TIsInteger = validation.isinteger.TIsInteger;
  TIsNumber = validation.isnumber.TIsNumber;
  TIsBoolean = validation.isBoolean.TIsBoolean;
  TIsDate = validation.isdate.TIsDate;
  TIsEnum = validation.isenum.TIsEnum;
  TIsObject = validation.isobject.TIsObject;
  TIsArray = validation.isarray.TIsArray;

implementation

end.




