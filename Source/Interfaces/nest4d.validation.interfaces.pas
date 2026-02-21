unit nest4d.validation.interfaces;

interface

uses
  Rtti,
  Generics.Collections,
  nest4d.request,
  System.Evolution.ResultPair;

type
  TResultValidation = TResultPair<Boolean, String>;

  IValidationArguments = interface
    ['{008AE8DA-AA34-4881-9477-617A0CD9B158}']
    function TagName: String;
    function FieldName: String;
    function Values: TArray<TValue>;
    function Message: String;
    function TypeName: String;
    function ObjectType: TClass;
  end;

  IValidatorConstraint = interface
    ['{56130D3B-C251-4F85-9215-937B08B17A43}']
    function Validate(const Value: TValue; const Args: IValidationArguments): TResultValidation;
  end;

  IValidationInfo = interface
    ['{8FCA8E1D-2244-46A2-9E7C-DB6F829EB6EE}']
    function _GetValidator: IValidatorConstraint;
    function _GetValidationArguments: IValidationArguments;
    function _GetValue: TValue;
    procedure _SetValidator(const Value: IValidatorConstraint);
    procedure _SetValidationArguments(const Value: IValidationArguments);
    procedure _SetValue(const Value: TValue);
    //
    property Validator: IValidatorConstraint read _GetValidator write _SetValidator;
    property Args: IValidationArguments read _GetValidationArguments write _SetValidationArguments;
    property Value: TValue read _GetValue write _SetValue;
  end;

  IValidationPipe = interface
    ['{9795C9EF-4FE3-422E-A237-C238E3935FD6}']
    function IsMessages: Boolean;
    function BuildMessages: String;
    procedure Validate(const AClass: TClass; const ARequest: IRouteRequest);
  end;

//  IValidatorOptions = interface
//    ['{6E078A2B-4E6A-4B27-B80A-18EB9C0EF27F}']
//    procedure SetOption(const APair: TPair<String, TValue>);
//    function GetOption(const AKey: String): TValue;
//  end;

implementation

end.







