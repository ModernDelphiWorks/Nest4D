unit validation.include;

interface

uses
  Rtti,
  Generics.Collections,
//  nest4d.decorator.body,
//  nest4d.decorator.param,
//  nest4d.decorator.query,
  decorator.isbase,
  validation.isString,
  validation.isinteger,
  validation.isempty,
  validation.isnotempty,
  nest4d.validation.arguments,
  nest4d.validation.interfaces;

type
  TConverter = TClass;
  TValidation = TClass;
  TObjectType = TClass;
  IValidationArguments = nest4d.validation.interfaces.IValidationArguments;
  TValidationArguments = nest4d.validation.arguments.TValidationArguments;
  IValidatorConstraint = nest4d.validation.interfaces.IValidatorConstraint;
//  TResultPair = nest4d.validation.interfaces.TResultValidation;
  TValue = Rtti.TValue;
//  TBody = decorator.body.TBody;
//  TParam = nest4d.decorator.param.Param;
//  TQuery = validation.query.TQuery;
//  IsBase = decorator.isbase.TIsBase;
  TIsEmpty = validation.isempty.TIsEmpty;
  TIsNotEmpty = validation.isnotempty.TIsNotEmpty;
  TIsString = validation.isString.TIsString;
  TIsInteger = validation.isinteger.TIsInteger;

implementation

end.


