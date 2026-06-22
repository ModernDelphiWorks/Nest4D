{
  ------------------------------------------------------------------------------
  Nidus
  Modular and scalable application framework for Delphi, inspired by the architectural patterns of NestJS.

  SPDX-License-Identifier: MIT
  Copyright (c) 2025-2026 Isaque Pinheiro

  Licensed under the MIT License.
  See the LICENSE file in the project root for full license information.
  ------------------------------------------------------------------------------
}

unit Nidus.Route.Parse;

interface

uses
  Rtti,
  Types,
  Classes,
  SysUtils,
  StrUtils,
  ModernSyntax.ResultPair,
  Nidus.Route.Abstract,
  Nidus.Route.Param,
  Nidus.Route.Service,
  Nidus.Route.Manager,
  Nidus.Request,
  Nidus.Exception,
  Nidus.Listener,
  Nidus.Inject,
  Nidus.Module;

type
  TReturnPair = TResultPair<TRouteAbstract, Exception>;
  TGuardCallback = TFunc<Boolean>;

  TRouteParse = class
  private
    FNidusInject: PNidusInject;
    FService: TRouteService;
    FRouteManager: TRouteManager;
    procedure _ResolveRoutes(const APath: string;
      const ACallback: TFunc<string, TReturnPair>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncludeRouteService(const AService: TRouteService);
    function SelectRoute(const APath: string;
      const AReq: IRouteRequest = nil): TReturnPair;
  end;

implementation

{ TRouteParse }

constructor TRouteParse.Create;
begin
  FNidusInject := GNidusInject;
  if not Assigned(FNidusInject) then
    raise EAppInjector.Create;
  FRouteManager := FNidusInject^.Get<TRouteManager>;
end;

destructor TRouteParse.Destroy;
begin
  FNidusInject := nil;
  FService.Free;
  inherited;
end;

procedure TRouteParse.IncludeRouteService(const AService: TRouteService);
begin
  FService := AService;
end;

function TRouteParse.SelectRoute(const APath: String;
  const AReq: IRouteRequest): TReturnPair;
var
  LArgs: TRouteParam;
  LPath: String;
  LRouteParts: String;
  LRouteResult: TReturnPair;
  LListener: TAppListener;
  LIndex: Integer;
  LFound: Boolean;
begin
  LPath := LowerCase(APath);
  if LPath = '' then
  begin
    Result.Failure(ERouteNotFoundException.CreateFmt('', [APath]));
    Exit;
  end;
  if FRouteManager.FindEndPoint(LPath) <> '' then
  begin
    LArgs := TRouteParam.Create(LPath, AReq);
    LRouteResult := FService.GetRoute(LArgs);
    LListener := FNidusInject^.Get<TAppListener>;
    if Assigned(LListener) then
      LListener.Execute(FormatListenerMessage(Format('[RoutesResolver] %s', [APath])));
  end
  else
  begin
    // DEC-051 — longest-literal-prefix probe. The exact FindEndPoint above missed
    // because the request path carries param VALUES (e.g. /bancos-carteiras/001/ZZ).
    // RemoveSuffix now stores composite-key routes under their literal prefix
    // (/bancos-carteiras), so trim trailing segments one at a time and retry
    // FindEndPoint, taking the LONGEST match. This generalises the previous
    // single-last-segment reduction to N trailing params; Horse then binds each
    // :param positionally once the route resolves and Next runs.
    LFound := False;
    LRouteParts := LPath;
    repeat
      LIndex := LRouteParts.LastIndexOf('/');
      if LIndex <= 0 then
        Break;
      LRouteParts := LRouteParts.Substring(0, LIndex);
      if FRouteManager.FindEndPoint(LRouteParts) <> '' then
      begin
        LArgs := TRouteParam.Create(LRouteParts, AReq);
        LRouteResult := FService.GetRoute(LArgs);
        LFound := True;
        Break;
      end;
    until LRouteParts = '';
    if not LFound then
      LRouteResult.Failure(ERouteNotFoundException.CreateFmt('', [APath]));
  end;
  Result := LRouteResult;
end;

procedure TRouteParse._ResolveRoutes(const APath: string;
  const ACallback: TFunc<string, TReturnPair>);
var
  LRoutes: TStringDynArray;
  LRoute: string;
  LResult: TReturnPair;
begin
  LRoutes := SplitString(APath, '/');
  for LRoute in LRoutes do
  begin
    if (LRoute = '') or (LRoute = '/') then
      Continue;
    if LRoute = LRoutes[High(LRoutes)] then
      Break;
    LResult := ACallback(LRoute);
  end;
end;

end.










