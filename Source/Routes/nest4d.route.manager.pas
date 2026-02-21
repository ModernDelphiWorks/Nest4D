unit Nest4D.Route.Manager;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  Evolution4D.Objects;

type
  TRouteManager = class
  private
    FEndPoints: TSmartPtr<TList<String>>;
  public
    constructor Create;
    function FindEndPoint(const ARoute: String): String;
    function RemoveSuffix(const ARoute: String): String;
    function EndPoints: TList<String>;
  end;

implementation

{ TRouteManager }

constructor TRouteManager.Create;
begin
  FEndPoints := TList<String>.Create;
end;

function TRouteManager.EndPoints: TList<String>;
begin
  Result := FEndPoints;
end;

function TRouteManager.FindEndPoint(const ARoute: String): String;
var
  LURI: String;
  LIndex: Integer;
begin
  Result := '';
  LURI := LowerCase(ARoute);
  LIndex := FEndpoints.AsRef.IndexOf(LURI);
  if LIndex > -1 then
    Result := FEndpoints.AsRef.Items[LIndex];
end;

function TRouteManager.RemoveSuffix(const ARoute: String): String;
const
  LPattern = '(/{[^/]*})|(/:[^/]+)$';
begin
  Result := TRegEx.Replace(ARoute, LPattern, '');
end;

end.
