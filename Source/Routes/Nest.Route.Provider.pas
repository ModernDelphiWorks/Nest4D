{
             Nest4D - Development Framework for Delphi


                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers?o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos ? permitido copiar e distribuir c?pias deste documento de
       licen?a, mas mud?-lo n?o ? permitido.

       Esta vers?o da GNU Lesser General Public License incorpora
       os termos e condi??es da vers?o 3 da GNU General Public License
       Licen?a, complementado pelas permiss?es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit Nest.Route.Provider;

interface

uses
  Rtti,
  SysUtils,
  Nest.Exception,
  Nest.Tracker,
  Nest.Module.Abstract,
  Nest.Injector,
  Nest.Listener,
  Nest.Route,
  Nest.Route.Abstract,
  Nest.Route.Param,
  Evolution.ResultPair,
  Evolution.Objects;

type
  TRouteProvider = class
  private
    FTracker: TTracker;
    FAppInjector: PAppInjector;
    FObjectEx: TEvolutionObject;
    function _RouteMiddleware(const ARoute: TRouteAbstract): TRouteAbstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncludeTracker(const ATracker: TTracker);
    function GetRoute(const AArgs: TRouteParam): TResultPair<TRouteAbstract, Exception>;
  end;

implementation

constructor TRouteProvider.Create;
begin
  FAppInjector := GAppInjector;
  if not Assigned(FAppInjector) then
    raise EAppInjector.Create;
  FObjectEx := FAppInjector^.Get<TEvolutionObject>;
end;

destructor TRouteProvider.Destroy;
begin
  FAppInjector := nil;
  if Assigned(FTracker) then
    FTracker := nil;
  inherited;
end;

procedure TRouteProvider.IncludeTracker(
  const ATracker: TTracker);
begin
  FTracker := ATracker;
end;

function TRouteProvider._RouteMiddleware(
  const ARoute: TRouteAbstract): TRouteAbstract;
var
  LMiddleware: IRouteMiddleware;
  LFor: Int16;
begin
  Result := ARoute;
  for LFor := 0 to High(ARoute.Middlewares) do
  begin
    LMiddleware := ARoute.Middlewares[LFor].Create;
    LMiddleware.After(ARoute);
  end;
end;

function TRouteProvider.GetRoute(const AArgs: TRouteParam): TResultPair<TRouteAbstract, Exception>;
var
  LListener: TAppListener;
begin
  Result.Success(FTracker.FindRoute(AArgs));
  if Result.ValueSuccess = nil then
    Exit;
  if not Assigned(Result.ValueSuccess.ModuleInstance) then
  begin
    Result.ValueSuccess.ModuleInstance := FObjectEx.Factory(Result.ValueSuccess.Module);
    LListener := FAppInjector^.Get<TAppListener>;
    if Assigned(LListener) then
      LListener.Execute(FormatListenerMessage(Format('[InstanceLoader] %s dependencies initialized', [Result.ValueSuccess.ModuleInstance.ClassName])));
  end;
  // Go to middleware events if they exist.
  _RouteMiddleware(Result.ValueSuccess);
end;

end.





