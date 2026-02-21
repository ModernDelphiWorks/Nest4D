unit nest4d.transform.interfaces;

interface

uses
  Rtti,
  Generics.Collections,
  nest4d.request,
  System.Evolution.ResultPair;

type
  TResultTransform = TResultPair<TValue, String>;
  TJsonMapped = TObjectDictionary<String, TList<TValue>>;

  ITransformArguments = interface
    ['{C410FE53-25D6-42DD-8D61-AF04E97C1628}']
    function TagName: String;
    function FieldName: String;
    function Values: TArray<TValue>;
    function Message: String;
    function ObjectType: TClass;
  end;

  ITransformPipe = interface
    ['{3E8A1756-3273-4FCA-87D8-242F92F588BD}']
    function Transform(const Value: TValue; const Metadata: ITransformArguments): TResultTransform;
  end;

  ITransformInfo = interface
    ['{8FCA8E1D-2244-46A2-9E7C-DB6F829EB6EE}']
    function _GetTransform: ITransformPipe;
    function _GetTransformArguments: ITransformArguments;
    function _GetValue: TValue;
    procedure _SetTransform(const Value: ITransformPipe);
    procedure _SetTransformArguments(const Value: ITransformArguments);
    procedure _SetValue(const Value: TValue);
    //
    property Transform: ITransformPipe read _GetTransform write _SetTransform;
    property Metadata: ITransformArguments read _GetTransformArguments write _SetTransformArguments;
    property Value: TValue read _GetValue write _SetValue;
  end;

implementation

end.




