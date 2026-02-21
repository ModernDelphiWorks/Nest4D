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

unit Nest4D.Validation.include;

interface

uses
  System.Rtti,
  System.Generics.Collections,
  Nest4D.Transform.Pipe,
  Nest4D.Transform.Arguments,
  Nest4D.Transform.Interfaces,
  Nest4D.Parse.Json.Pipe,
  Nest4D.Parse.Integer.Pipe,
  Nest4D.Validation.Arguments,
  Nest4D.Validator.Constraint,
  Validation.isString,
  Validation.isinteger,
  Validation.isempty,
  Validation.isnotempty,
  Validation.isarray,
  Validation.isobject,
  Validation.isnumber,
  Validation.isdate,
  Validation.isBoolean,
  Validation.isenum,
  Nest4D.Validation.Interfaces;

type
  TResultValidation = Nest4D.Validation.Interfaces.TResultValidation;
  TResultTransform = Nest4D.Transform.interfaces.TResultTransform;
  TJsonMapped = Nest4D.Transform.interfaces.TJsonMapped;
  //
  TTransformPipe = Nest4D.Transform.pipe.TTransformPipe;
  ITransformArguments = Nest4D.Transform.interfaces.ITransformArguments;
  TTransformArguments = Nest4D.Transform.arguments.TTransformArguments;
  //
  IValidationArguments = Nest4D.Validation.Interfaces.IValidationArguments;
  IValidatorConstraint = Nest4D.Validation.Interfaces.IValidatorConstraint;
  IValidationInfo = Nest4D.Validation.Interfaces.IValidationInfo;
  IValidationPipe = Nest4D.Validation.Interfaces.IValidationPipe;
  ITransformInfo = Nest4D.Transform.interfaces.ITransformInfo;
  ITransformPipe = Nest4D.Transform.interfaces.ITransformPipe;
  //
  TValidationArguments = Nest4D.Validation.arguments.TValidationArguments;
  TValidatorConstraint = Nest4D.validator.constraint.TValidatorConstraint;
  //
  TParseJsonPipe = Nest4D.parse.json.pipe.TParseJsonPipe;
  TParseIntegerPipe = Nest4D.parse.integer.pipe.TParseIntegerPipe;
  //
  TIsEmpty = Validation.isempty.TIsEmpty;
  TIsNotEmpty = Validation.isnotempty.TIsNotEmpty;
  TIsString = Validation.isString.TIsString;
  TIsInteger = Validation.isinteger.TIsInteger;
  TIsNumber = Validation.isnumber.TIsNumber;
  TIsBoolean = Validation.isBoolean.TIsBoolean;
  TIsDate = Validation.isdate.TIsDate;
  TIsEnum = Validation.isenum.TIsEnum;
  TIsObject = Validation.isobject.TIsObject;
  TIsArray = Validation.isarray.TIsArray;

implementation

end.




