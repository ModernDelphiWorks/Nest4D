unit nfebr.lib.jsonbr;

interface

uses
  Generics.Collections,
  SysUtils,
  jsonbr.builders,
  jsonbr;

type
  TJsonBrLib = class
  public
    function FromJson<T: class, constructor>(const Json: String): T;
    function FromJsonList<T: class, constructor>(const Json: String): TObjectList<T>;
    function ToJson<T: class, constructor>(const Value: T): String;
    function ToJsonList<T: class, constructor>(const Value: TObjectList<T>): String;
  end;

implementation

{ TJsonBrLib }

function TJsonBrLib.FromJson<T>(const Json: String): T;
begin
  Result := TJSONBr.JsonToObject<T>(Json);
end;

function TJsonBrLib.FromJsonList<T>(const Json: String): TObjectList<T>;
begin
  Result := TJSONBr.JsonToObjectList<T>(Json);
end;

function TJsonBrLib.ToJson<T>(const Value: T): String;
begin
  Result := TJSONBr.ObjectToJsonString(Value);
end;

function TJsonBrLib.ToJsonList<T>(const Value: TObjectList<T>): String;
begin
  Result := TJSONBr.ObjectListToJsonString(TObjectList<TObject>(Value));
end;

end.


