unit Validation.include;

interface

uses
  System.Rtti,
  System.Generics.Collections,
  Decorator.IsBase,
  Validation.IsString,
  Validation.IsInteger,
  Validation.IsEmpty,
  Validation.IsNotEmpty,
  Nest4D.Validation.Arguments,
  Nest4D.Validation.Interfaces;

type
  TConverter = TClass;
  TValidation = TClass;
  TObjectType = TClass;
  IValidationArguments = Nest4D.Validation.Interfaces.IValidationArguments;
  TValidationArguments = Nest4D.Validation.Arguments.TValidationArguments;
  IValidatorConstraint = Nest4D.Validation.Interfaces.IValidatorConstraint;
  TValue = System.Rtti.TValue;
  TIsEmpty = Validation.IsEmpty.TIsEmpty;
  TIsNotEmpty = Validation.IsNotEmpty.TIsNotEmpty;
  TIsString = Validation.IsString.TIsString;
  TIsInteger = Validation.IsInteger.TIsInteger;

implementation

end.






