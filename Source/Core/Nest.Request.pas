unit Nest.Request;

interface

uses
  Rtti,
  SysUtils,
  Classes,
  EncdDecd,
  Generics.Collections,
  Nest.Request.Data;

type
  IRouteRequest = interface
    ['{F344E29D-8FF3-4F39-BC8C-53EE130E02D4}']
    procedure SetObject(const AObject: TObject);
    procedure SetBody(const ABody: String);
    function Header: TRequestData;
    function Querys: TRequestData;
    function Params: TRequestData;
    function Method: String;
    function Body: String;
    function URL: String;
    function Host: String;
    function Port: integer;
    function ContentType: String;
    function Authorization: String;
    function AsObject: TObject;
  end;

  TRouteRequest = class(TInterfacedObject, IRouteRequest)
  private
    FObject: TObject;
    FHeader: TRequestData;
    FParams: TRequestData;
    FQuerys: TRequestData;
    FMethod: String;
    FURL: String;
    FBody: String;
    FHost: String;
    FPort: integer;
    FContentType: String;
    FAuthorization: String;
  public
    constructor Create(const AHeader: TStrings; const AParams: TStrings;
      const AQuerys: TStrings; const ABody: String; const AHost: String;
      const AContentType: String; const AMethod: String;
      const AURL: String; const APort: integer; const AAuthorization: String);
    destructor Destroy; override;
    procedure SetObject(const AObject: TObject);
    procedure SetBody(const ABody: String);
    procedure SetAuthorization(const AAuthorization: String);
    function Header: TRequestData;
    function Params: TRequestData;
    function Querys: TRequestData;
    function Body: String;
    function Host: String;
    function ContentType: String;
    function Method: String;
    function URL: String;
    function Port: integer;
    function Authorization: String;
    function AsObject: TObject;
  end;

implementation

{ TRouteRequest }

function TRouteRequest.Body: String;
begin
  Result := FBody;
end;

function TRouteRequest.ContentType: String;
begin
  Result := FContentType;
end;

constructor TRouteRequest.Create(const AHeader: TStrings; const AParams: TStrings;
  const AQuerys: TStrings; const ABody: String; const AHost: String;
  const AContentType: String; const AMethod: String;
  const AURL: String; const APort: integer; const AAuthorization: String);
begin
  FHeader := TRequestData.Create;
  FParams := TRequestData.Create;
  FQuerys := TRequestData.Create;
  FHeader.Assign(AHeader);
  FParams.Assign(AParams);
  FQuerys.Assign(AQuerys);
  FBody := ABody;
  FHost := AHost;
  FMethod := AMethod;
  FURL := AURL;
  FPort := APort;
  FContentType := AContentType;
  FAuthorization := AAuthorization;
end;

destructor TRouteRequest.Destroy;
begin
  FHeader.Free;
  FParams.Free;
  FQuerys.Free;
  if Assigned(FObject) then
    FObject.Free;
  inherited;
end;

function TRouteRequest.Header: TRequestData;
begin
  Result := FHeader;
end;

function TRouteRequest.Host: String;
begin
  Result := FHost;
end;

function TRouteRequest.Method: String;
begin
  Result := FMethod;
end;

function TRouteRequest.AsObject: TObject;
begin
  Result := FObject;
end;

function TRouteRequest.Params: TRequestData;
begin
  Result := FParams;
end;

function TRouteRequest.Port: integer;
begin
  Result := FPort;
end;

function TRouteRequest.Querys: TRequestData;
begin
  Result := FQuerys;
end;

procedure TRouteRequest.SetBody(const ABody: String);
begin
  FBody := ABody;
end;

procedure TRouteRequest.SetObject(const AObject: TObject);
begin
  FObject := AObject;
end;

procedure TRouteRequest.SetAuthorization(const AAuthorization: String);
begin
  FAuthorization := AAuthorization;
end;

function TRouteRequest.Authorization: String;
begin
  Result := FAuthorization;
end;

function TRouteRequest.URL: String;
begin
  Result := FURL;
end;

end.





