unit Decorator.Include;

interface

uses
  Nest4D.Decorator.Param,
  Nest4D.Decorator.Body,
  Nest4D.Decorator.Query,
  Decorator.IsBase,
  Decorator.isstring,
  Decorator.IsInteger,
  Decorator.IsNotEmpty,
  Decorator.IsBoolean,
  Decorator.IsNumber,
  Decorator.IsObject,
  Decorator.IsArray,
  Decorator.IsDate,
  Decorator.IsEnum,
  Decorator.IsEmpty,
  Decorator.IsMax,
  Decorator.IsMin,
  Decorator.IsMinLength,
  Decorator.IsMaxLength,
  Decorator.IsAlpha,
  Decorator.IsAlphanumeric,
  Decorator.Contains,
  Decorator.islength;

type
  ParamAttribute = Nest4D.Decorator.param.ParamAttribute;
  QueryAttribute = Nest4D.Decorator.query.QueryAttribute;
  BodyAttribute = Nest4D.Decorator.body.BodyAttribute;
  IsAttribute = Decorator.IsBase.IsAttribute;
  IsEmptyAttribute = Decorator.isempty.IsEmptyAttribute;
  IsNotEmptyAttribute = Decorator.isnotempty.IsNotEmptyAttribute;
  IsStringAttribute = Decorator.isString.IsStringAttribute;
  IsIntegerAttribute = Decorator.isinteger.IsIntegerAttribute;
  IsBooleanAttribute = Decorator.isBoolean.IsBooleanAttribute;
  IsNumberAttribute = Decorator.isnumber.IsnumberAttribute;
  IsObjectAttribute = Decorator.isobject.IsObjectAttribute;
  IsArrayAttribute = Decorator.isarray.IsArrayAttribute;
  IsEnumAttribute = Decorator.isenum.IsEnumAttribute;
  IsDateAttribute = Decorator.isdate.IsDateAttribute;
  IsMinAttribute = Decorator.ismin.IsMinAttribute;
  IsMaxAttribute = Decorator.ismax.IsMaxAttribute;
  IsMinLengthAttribute = Decorator.isminlength.IsMinLengthAttribute;
  IsMaxLengthAttribute = Decorator.ismaxlength.IsMaxLengthAttribute;
  IsLengthAttribute = Decorator.islength.IsLengthAttribute;
  IsAlphaAttribute = Decorator.isalpha.IsAlphaAttribute;
  IsAlphaNumericAttribute = Decorator.isalphanumeric.IsAlphaNumericAttribute;
  ContainsAttribute = Decorator.contains.ContainsAttribute;

implementation

end.




