unit Decorator.IsBase;

interface

uses
  System.SysUtils,
  System.Rtti,
  Nest4D.Validation.types;

type
  IsAttribute = class(TCustomAttribute)
  protected
    FTagName: String;
    FMessage: String;
  public
    constructor Create(const AMessage: String = ''); virtual;
    function TagName: String;
    function Message: String;
    function Validation: TValidation; virtual; abstract;
    function Params: TArray<TValue>; virtual;
  end;

implementation

{ IsAttribute }

constructor IsAttribute.Create(const AMessage: String);
begin
  FTagName := '';
  FMessage := AMessage;
end;

function IsAttribute.Message: String;
begin
  Result := FMessage;
end;

function IsAttribute.Params: TArray<TValue>;
begin
  Result := [];
end;

function IsAttribute.TagName: String;
begin
  Result := FTagName;
end;

end.



