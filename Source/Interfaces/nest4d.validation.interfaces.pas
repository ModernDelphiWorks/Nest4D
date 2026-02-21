unit Nest4D.Validation.Interfaces;

interface

uses
  System.Rtti,
  System.Generics.Collections,
  Nest4D.Request,
  Evolution4D.ResultPair;

type
  TResultValidation = TResultPair<Boolean, String>;
  TJsonMapped = TObjectDictionary<String, TList<TValue>>;

  IValidationArguments = interface
    ['{8B2F4E1A-9C3D-4F5E-A7B6-1D2E3F4A5B6C}']
    function TagName: String;
    function FieldName: String;
    function Values: TArray<TValue>;
    function Message: String;
    function TypeName: String;
    function ObjectType: TClass;
  end;

  IValidatorConstraint = interface
    ['{7A1B2C3D-4E5F-6A7B-8C9D-0E1F2A3B4C5D}']
    function Validate(const Value: TValue; const Arguments: IValidationArguments): TResultValidation;
  end;

  IValidationInfo = interface
    ['{6F8E9D0C-1B2A-3948-5766-8594A3B2C1D0}']
    function _GetValidator: IValidatorConstraint;
    function _GetValidationArguments: IValidationArguments;
    function _GetValue: TValue;
    procedure _SetValidator(const Value: IValidatorConstraint);
    procedure _SetValidationArguments(const Value: IValidationArguments);
    procedure _SetValue(const Value: TValue);
    //
    property Validator: IValidatorConstraint read _GetValidator write _SetValidator;
    property Arguments: IValidationArguments read _GetValidationArguments write _SetValidationArguments;
    property Value: TValue read _GetValue write _SetValue;
  end;

  IValidationPipe = interface
    ['{5E7D6C4B-3A29-1847-6355-7483B2A1C0D9}']
    function IsMessages: Boolean;
    function BuildMessages: String;
    procedure Validate(const AClass: TClass; const ARequest: IRouteRequest);
//    function Validate(const Value: TValue; const Metadata: IRouteRequest): TResultValidation;
  end;

implementation

end.










