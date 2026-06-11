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

unit Nidus.Tracker;

interface

uses
  Rtti,
  Types,
  Classes,
  SysUtils,
  RegularExpressions,
  Generics.Collections,
  ModernSyntax.Objects,
  Nidus.Module.Abstract,
  Nidus.Exception,
  Nidus.Route.Param,
  Nidus.Route.Key,
  Nidus.Route.Abstract,
  Nidus.Route.Manager,
  Nidus.Bind,
  Nidus.Inject,
  Nidus.Request;

type
  TTrackerRoute = TObjectDictionary<TRouteKey, TRouteAbstract>;

  TTracker = class
  private
    FNidusInject: PNidusInject;
    FAppModule: TModuleAbstract;
    FRoutes: TTrackerRoute;
    FAppIntialPath: string;
    FCurrentPath: string;
    FRouteManager: TRouteManager;
    FRequest: IRouteRequest;
    FObject: TModernObject;
    // Visited module classes within a single BindModule resolution — dedupes
    // diamond imports so an exported singleton (e.g. TJsonAdapter from
    // TCoreModule) is registered into the module injector exactly once even
    // when two import paths reach the same module.
    FVisitedImports: TStringList;
    procedure _AddModuleBinds(const AModule: TModuleAbstract;
      const AInject: TNidusInject);
    procedure _AddExportedModuleBinds(const AModule: TModuleAbstract;
      const AInject: TNidusInject);
    procedure _AddModuleImportsBind(const AModule: TModuleAbstract;
      const AInject: TNidusInject);
    procedure _AddRoute(const ARoute: TRouteAbstract; const AParent: string);
    procedure _ResolverImports(const AModule: TClass;
      const AInject: TNidusInject);
    function _CreateInjector: TNidusInject;
    function _CreateModule(const AModule: TClass): TModuleAbstract;
    procedure _GuardianRoute(const ARoute: TRouteAbstract);
    function _RouteMiddlewares(const ARoute: TRouteAbstract): TRouteAbstract;
    procedure _RemoveEndPoint(const APath: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RunApp(const AModule: TModuleAbstract; const AIntialRoutePath: string);
    procedure BindModule(const AModule: TModuleAbstract);
    procedure RemoveRoutes(const AModuleName: string);
    procedure AddRoutes(const AModule: TModuleAbstract);
    procedure ExtractInject<T: class>(const ATag: string);
    function GetBind<T: class, constructor>(const ATag: string): T;
    function GetBindInterface<I: IInterface>(const ATag: string): I;
    function FindRoute(const AArgs: TRouteParam): TRouteAbstract;
    function DisposeModule(const AArgs: TRouteParam): TRouteAbstract;
    function GetModule: TModuleAbstract;
    function CurrentPath: string;
  end;

implementation

uses
  Nidus.Listener;

{ TTracker }

constructor TTracker.Create;
begin
  FRoutes := TTrackerRoute.Create([doOwnsValues]);
  FVisitedImports := TStringList.Create;
  FVisitedImports.Sorted := True;
  FVisitedImports.Duplicates := dupIgnore;
  FNidusInject := GNidusInject;
  if not Assigned(FNidusInject) then
    raise EAppInjector.Create;
  FRouteManager := FNidusInject^.Get<TRouteManager>;
  FObject := FNidusInject^.Get<TModernObject>
end;

destructor TTracker.Destroy;
begin
  FNidusInject := nil;
  FRouteManager := nil;
  FAppModule := nil;
  FObject := nil;
  FVisitedImports.Free;
  FRoutes.Free;
  inherited;
end;

function TTracker.DisposeModule(const AArgs: TRouteParam): TRouteAbstract;
var
  LKey: TRouteKey;
  LEndPoint: String;
begin
  Result := nil;
  LEndPoint := FRouteManager.FindEndpoint(AArgs.Path);
  if LEndPoint = '' then
    Exit;
  for LKey in FRoutes.Keys do
  begin
    if LKey.Path <> LEndPoint then
      Continue;
    Result := FRoutes.Items[LKey];
    Break;
  end;
end;

procedure TTracker.ExtractInject<T>(const ATag: String);
begin
  FNidusInject^.ExtractInject<T>(ATag);
end;

procedure TTracker._AddModuleBinds(const AModule: TModuleAbstract;
  const AInject: TNidusInject);
var
  LBind: TBind<TObject>;
begin
  for LBind in AModule.Binds do
  begin
    LBind.IncludeInject(AInject);
    LBind.Free;
  end;
end;

procedure TTracker._AddExportedModuleBinds(const AModule: TModuleAbstract;
  const AInject: TNidusInject);
var
  LBind: TBind<TObject>;
  LExportedBinds: TExportedBinds;
begin
  LExportedBinds := AModule.ExportedBinds;
  if Length(LExportedBinds) = 0 then
    Exit;
  for LBind in LExportedBinds do
  begin
    LBind.IncludeInject(AInject);
    LBind.Free;
  end;
end;

procedure TTracker._AddModuleImportsBind(const AModule: TModuleAbstract;
  const AInject: TNidusInject);
var
  LModule: TClass;
begin
  _AddExportedModuleBinds(AModule, AInject);
  if Length(AModule.Imports) = 0 then
    Exit;
  for LModule in AModule.Imports do
    _ResolverImports(LModule, AInject);
end;

procedure TTracker._AddRoute(const ARoute: TRouteAbstract; const AParent: String);
var
  LPath: String;
begin
  LPath := FRouteManager.RemoveSuffix(LowerCase(ARoute.Path));
  FRoutes.AddOrSetValue(TRouteKey.Create(LPath, AParent), ARoute);
  FRouteManager.EndPoints.Add(LPath);
  FRouteManager.EndPoints.Sort;
end;

function TTracker._CreateInjector: TNidusInject;
begin
  Result := TNidusInject.Create;
end;

function TTracker._CreateModule(const AModule: TClass): TModuleAbstract;
begin
  Result := FObject.Factory(AModule) as TModuleAbstract;
end;

procedure TTracker._GuardianRoute(const ARoute: TRouteAbstract);
var
  LMiddleware: IRouteMiddleware;
  LParamRequest: TValue;
  LFor: Int16;
begin
  if Length(ARoute.Middlewares) = 0 then
    Exit;
  for LFor := 0 to High(ARoute.Middlewares) do
  begin
    LMiddleware := ARoute.Middlewares[LFor].Create;
    LParamRequest := TValue.From<IRouteRequest>(FRequest);
//    if LParamRequest.AsInterface = nil then
//      Continue;
    if not LMiddleware.Call(LParamRequest.AsType<IRouteRequest>) then
      raise EUnauthorizedException.Create('');
  end;
end;

procedure TTracker.AddRoutes(const AModule: TModuleAbstract);
var
  LFor: Int16;
  LRoutes: TRoutes;
begin
  LRoutes := AModule.Routes;
  for LFor := 0 to High(LRoutes) do
    _AddRoute(LRoutes[LFor] as TRouteAbstract, AModule.ClassName);
end;

procedure TTracker.BindModule(const AModule: TModuleAbstract);
var
  LInjector: TNidusInject;
  LModule: TClass;
begin
  // O Bind dos módulos são efetuados por rota, se várias rotas usarem o mesmo
  // módulo, deve gerar somente um injector para o módulo, independente de
  // quantas rotas chama-lo.
  LInjector := FNidusInject^.Get<TNidusInject>(AModule.ClassName);
  if LInjector <> nil then
    Exit;

  // Injector do Modulo
  LInjector := _CreateInjector;
  LInjector.Name := AModule.ClassName;
  // Reset the diamond-import dedup set for this module injector build.
  FVisitedImports.Clear;
  FVisitedImports.Add(AModule.ClassName);
  _AddModuleBinds(AModule, LInjector);
  if Length(AModule.Imports) > 0 then
  begin
    for LModule in AModule.Imports do
      _ResolverImports(LModule, LInjector);
  end;
  // Adiciona ao AppInjector
  FNidusInject^.AddInject(AModule.ClassName, LInjector);
end;

function TTracker.CurrentPath: String;
begin
  Result := FCurrentPath;
end;

function TTracker.FindRoute(const AArgs: TRouteParam): TRouteAbstract;
var
  LKey: TRouteKey;
  LEndPoint: string;
  LRoute: TRouteAbstract;
begin
  // Request atualizada a cada requisição, para ser usada internamente
  FRequest := AArgs.Request;
  Result := nil;
  LEndPoint := FRouteManager.FindEndpoint(AArgs.Path);
  if LEndPoint = '' then
    Exit;
  for LKey in FRoutes.Keys do
  begin
    if LKey.Path <> LEndPoint then
      Continue;
    LRoute := FRoutes.Items[LKey];
    _GuardianRoute(LRoute);
    Result := _RouteMiddlewares(LRoute);
    Break;
  end;
end;

function TTracker.GetBind<T>(const ATag: string): T;
begin
  Result := FNidusInject^.Get<T>(ATag);
end;

function TTracker.GetBindInterface<I>(const ATag: string): I;
begin
  Result := FNidusInject^.GetInterface<I>(ATag);
end;

function TTracker.GetModule: TModuleAbstract;
begin
  if not Assigned(FAppModule) then
    raise EModuleStartedInitException.Create('');
  Result := FAppModule;
end;

procedure TTracker._RemoveEndPoint(const APath: String);
begin
  FRouteManager.EndPoints.Remove(LowerCase(APath));
  FRouteManager.EndPoints.Sort;
end;

procedure TTracker.RemoveRoutes(const AModuleName: String);
var
  LKey: TRouteKey;
begin
  for LKey in FRoutes.Keys do
  begin
    if LKey.Schema <> AModuleName then
      Continue;
    // Remove the endpoints of this module from the list.
    _RemoveEndPoint(LKey.Path);
    // Remove all routes/sub-routes of the module being destroyed.
    FRoutes.Remove(LKey);
  end;
end;

procedure TTracker.RunApp(const AModule: TModuleAbstract;
  const AIntialRoutePath: String);
begin
  FAppIntialPath := AIntialRoutePath;
  FCurrentPath := AIntialRoutePath;
  FAppModule := AModule;
end;

procedure TTracker._ResolverImports(const AModule: TClass;
  const AInject: TNidusInject);
var
  LInstance: TModuleAbstract;
begin
  // Diamond-import guard: a module reached through more than one import path is
  // resolved (and its exported binds registered) only once per injector build.
  if FVisitedImports.IndexOf(AModule.ClassName) >= 0 then
    Exit;
  FVisitedImports.Add(AModule.ClassName);
  LInstance := _CreateModule(AModule);
  if LInstance = nil then
    Exit;
  try
    _AddModuleImportsBind(LInstance, AInject);
  finally
    LInstance.Free;
  end;
end;

function TTracker._RouteMiddlewares(const ARoute: TRouteAbstract): TRouteAbstract;
var
  LMiddleware: IRouteMiddleware;
  LFor: Int16;
begin
  Result := ARoute;
  for LFor := 0 to High(ARoute.Middlewares) do
  begin
    LMiddleware := ARoute.Middlewares[LFor].Create;
    Result := LMiddleware.Before(ARoute);
  end;
end;

end.

