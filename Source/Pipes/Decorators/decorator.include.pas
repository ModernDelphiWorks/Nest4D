unit decorator.include;

interface

uses
  nest4d.decorator.param,
  nest4d.decorator.body,
  nest4d.decorator.query,
  decorator.isbase,
  decorator.isString,
  decorator.isinteger,
  decorator.isnotempty,
  decorator.isBoolean,
  decorator.isnumber,
  decorator.isobject,
  decorator.isarray,
  decorator.isdate,
  decorator.isenum,
  decorator.isempty,
  decorator.ismax,
  decorator.ismin,
  decorator.isminlength,
  decorator.ismaxlength,
  decorator.isalpha,
  decorator.isalphanumeric,
  decorator.contains,
  decorator.islength;

type
  ParamAttribute = nest4d.decorator.param.ParamAttribute;
  QueryAttribute = nest4d.decorator.query.QueryAttribute;
  BodyAttribute = nest4d.decorator.body.BodyAttribute;
  IsAttribute = decorator.isbase.IsAttribute;
  IsEmptyAttribute = decorator.isempty.IsEmptyAttribute;
  IsNotEmptyAttribute = decorator.isnotempty.IsNotEmptyAttribute;
  IsStringAttribute = decorator.isString.IsStringAttribute;
  IsIntegerAttribute = decorator.isinteger.IsIntegerAttribute;
  IsBooleanAttribute = decorator.isBoolean.IsBooleanAttribute;
  IsNumberAttribute = decorator.isnumber.IsnumberAttribute;
  IsObjectAttribute = decorator.isobject.IsObjectAttribute;
  IsArrayAttribute = decorator.isarray.IsArrayAttribute;
  IsEnumAttribute = decorator.isenum.IsEnumAttribute;
  IsDateAttribute = decorator.isdate.IsDateAttribute;
  IsMinAttribute = decorator.ismin.IsMinAttribute;
  IsMaxAttribute = decorator.ismax.IsMaxAttribute;
  IsMinLengthAttribute = decorator.isminlength.IsMinLengthAttribute;
  IsMaxLengthAttribute = decorator.ismaxlength.IsMaxLengthAttribute;
  IsLengthAttribute = decorator.islength.IsLengthAttribute;
  IsAlphaAttribute = decorator.isalpha.IsAlphaAttribute;
  IsAlphaNumericAttribute = decorator.isalphanumeric.IsAlphaNumericAttribute;
  ContainsAttribute = decorator.contains.ContainsAttribute;

implementation

end.




