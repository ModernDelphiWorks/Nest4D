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

unit Nest4D.Route.Provider;

interface

uses
  System.Rtti,
  System.SysUtils,
  Nest4D.Exception,
  Nest4D.Tracker,
  Nest4D.Module.Abstract,
  Nest4D.Injector,
  Nest4D.Listener,
  Nest4D.Route,
  Nest4D.Route.Abstract,
  Nest4D.Route.Param,
  Nest4D.Pool.Config,
  Evolution4D.ResultPair,
  Evolution4D.Objects;

type
  TRouteProvider = class
  private
    FTracker: TTracker;
    FAppInjector: PAppInjector;
    FObjectEx: TEvolutionObject;
    FPoolConfig: TPoolConfig;
    function _RouteMiddleware(const ARoute: TRouteAbstract): TRouteAbstract;
    function _GetModuleFromPool(const AModuleClass: TClass): TModuleAbstract;
    procedure _ReturnModuleToPool(const AModule: TModuleAbstract; const AModuleClass: TClass);
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
  FPoolConfig := TPoolConfig.Create;
end;

destructor TRouteProvider.Destroy;
begin
  FAppInjector := nil;
  if Assigned(FTracker) then
    FTracker := nil;
  if Assigned(FPoolConfig) then
    FPoolConfig.Free;
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
  LNewModuleCreated: Boolean;
begin
  Result.Success(FTracker.FindRoute(AArgs));
  if Result.ValueSuccess = nil then
    Exit;
  if not Assigned(Result.ValueSuccess.ModuleInstance) then
  begin
    LNewModuleCreated := False;
    
    // Verificar se deve usar pool
    if Result.ValueSuccess.UsePool then
    begin
      Result.ValueSuccess.ModuleInstance := _GetModuleFromPool(Result.ValueSuccess.Module);
      if not Assigned(Result.ValueSuccess.ModuleInstance) then
      begin
        // Criar novo módulo e adicionar ao pool
        Result.ValueSuccess.ModuleInstance := FObjectEx.Factory(Result.ValueSuccess.Module);
        LNewModuleCreated := True;
        
        // Adicionar o módulo recém-criado ao pool para uso futuro
        if Assigned(FPoolConfig) then
          FPoolConfig.AddModule(Result.ValueSuccess.Module.ClassName, TModuleAbstract(Result.ValueSuccess.ModuleInstance));
      end;
    end
    else
    begin
      Result.ValueSuccess.ModuleInstance := FObjectEx.Factory(Result.ValueSuccess.Module);
      LNewModuleCreated := True;
    end;
    
    LListener := FAppInjector^.Get<TAppListener>;
    if Assigned(LListener) then
    begin
      if LNewModuleCreated then
        LListener.Execute(FormatListenerMessage(Format('[InstanceLoader] %s dependencies initialized', [Result.ValueSuccess.ModuleInstance.ClassName])))
      else
        LListener.Execute(FormatListenerMessage(Format('[PoolLoader] %s retrieved from pool', [Result.ValueSuccess.ModuleInstance.ClassName])));
    end;
  end;
  // Go to middleware events if they exist.
  _RouteMiddleware(Result.ValueSuccess);
end;

function TRouteProvider._GetModuleFromPool(const AModuleClass: TClass): TModuleAbstract;
begin
  Result := nil;
  if Assigned(FPoolConfig) then
    Result := FPoolConfig.GetModule(AModuleClass.ClassName);
end;

procedure TRouteProvider._ReturnModuleToPool(const AModule: TModuleAbstract; const AModuleClass: TClass);
begin
  if Assigned(FPoolConfig) and Assigned(AModule) then
    FPoolConfig.ReturnModule(AModuleClass.ClassName, AModule);
end;

end.






