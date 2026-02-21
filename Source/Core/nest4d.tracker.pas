{
             Nest4D - Development Framework for Delphi


                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit nest4d.tracker;

interface

uses
  Rtti,
  Types,
  SysUtils,
  RegularExpressions,
  Generics.Collections,
  nest4d.module.abstract,
  nest4d.exception,
  nest4d.route.param,
  nest4d.route.key,
  nest4d.route.abstract,
  nest4d.route.manager,
  System.Evolution.Objects,
  nest4d.bind,
  nest4d.injector,
  nest4d.request;

type
  TTrackerRoute = TObjectDictionary<TRouteKey, TRouteAbstract>;

  TTracker = class
  private
    FAppInjector: PAppInjector;
    FAppModule: TModuleAbstract;
    FRoutes: TTrackerRoute;
    FAppIntialPath: String;
    FCurrentPath: String;
    FRouteManager: TRouteManager;
    FRequest: IRouteRequest;
    FObjectEx: TEvolutionObject;
    procedure _AddModuleBinds(const AModule: TModuleAbstract;
      const AInjector: TAppInjector);
    procedure _AddExportedModuleBinds(const AModule: TModuleAbstract;
      const AInjector: TAppInjector);
    procedure _AddModuleImportsBind(const AModule: TModuleAbstract;
      const AInjector: TAppInjector);
    procedure _AddRoute(const ARoute: TRouteAbstract; const AParent: String);
    procedure _ResolverImports(const AModule: TClass;
      const AInjector: TAppInjector);
    function _CreateInjector: TAppInjector;
    function _CreateModule(const AModule: TClass): TModuleAbstract;
    procedure _GuardianRoute(const ARoute: TRouteAbstract);
    function _RouteMiddlewares(const ARoute: TRouteAbstract): TRouteAbstract;
    procedure _RemoveEndPoint(const APath: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RunApp(const AModule: TModuleAbstract; const AIntialRoutePath: String);
    procedure BindModule(const AModule: TModuleAbstract);
    procedure RemoveRoutes(const AModuleName: String);
    procedure AddRoutes(const AModule: TModuleAbstract);
    procedure ExtractInjector<T: class>(const ATag: String);
    function GetBind<T: class, constructor>(const ATag: String): T;
    function GetBindInterface<I: IInterface>(const ATag: String): I;
    function FindRoute(const AArgs: TRouteParam): TRouteAbstract;
    function DisposeModule(const AArgs: TRouteParam): TRouteAbstract;
    function GetModule: TModuleAbstract;
    function CurrentPath: String;
  end;

implementation

uses
  nest4d.listener;

{ TTracker }

constructor TTracker.Create;
begin
  FRoutes := TTrackerRoute.Create([doOwnsValues]);
  FAppInjector := GAppInjector;
  if not Assigned(FAppInjector) then
    raise EAppInjector.Create;
  FRouteManager := FAppInjector^.Get<TRouteManager>;
  FObjectEx := FAppInjector^.Get<TEvolutionObject>
end;

destructor TTracker.Destroy;
begin
  FAppInjector := nil;
  FRouteManager := nil;
  FAppModule := nil;
  FObjectEx := nil;
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

procedure TTracker.ExtractInjector<T>(const ATag: String);
begin
  FAppInjector^.ExtractInjector<T>(ATag);
end;

procedure TTracker._AddModuleBinds(const AModule: TModuleAbstract;
  const AInjector: TAppInjector);
var
  LBind: TBind<TObject>;
begin
  for LBind in AModule.Binds do
  begin
    LBind.IncludeInjector(AInjector);
    LBind.Free;
  end;
end;

procedure TTracker._AddExportedModuleBinds(const AModule: TModuleAbstract;
  const AInjector: TAppInjector);
var
  LBind: TBind<TObject>;
  LExportedBinds: TExportedBinds;
begin
  LExportedBinds := AModule.ExportedBinds;
  if Length(LExportedBinds) = 0 then
    Exit;
  for LBind in LExportedBinds do
  begin
    LBind.IncludeInjector(AInjector);
    LBind.Free;
  end;
end;

procedure TTracker._AddModuleImportsBind(const AModule: TModuleAbstract;
  const AInjector: TAppInjector);
var
  LModule: TClass;
begin
  _AddExportedModuleBinds(AModule, AInjector);
  if Length(AModule.Imports) = 0 then
    Exit;
  for LModule in AModule.Imports do
    _ResolverImports(LModule, AInjector);
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

function TTracker._CreateInjector: TAppInjector;
begin
  Result := TAppInjector.Create;
end;

function TTracker._CreateModule(const AModule: TClass): TModuleAbstract;
begin
  Result := FObjectEx.Factory(AModule) as TModuleAbstract;
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
  LInjector: TAppInjector;
  LModule: TClass;
begin
  // O Bind dos m�dulos s�o efetuados por rota, se v�rias rotas usarem o mesmo
  // m�dulo, deve gerar somente um injector para o m�dulo, independente de
  // quantas rotas chama-lo.
  LInjector := FAppInjector^.Get<TAppInjector>(AModule.ClassName);
  if LInjector <> nil then
    Exit;
  // Injector do Modulo
  LInjector := _CreateInjector;
  _AddModuleBinds(AModule, LInjector);
  if Length(AModule.Imports) > 0 then
  begin
    for LModule in AModule.Imports do
      _ResolverImports(LModule, LInjector);
  end;
  // Adiciona ao AppInjector
  FAppInjector^.AddInjector(AModule.ClassName, LInjector);
end;

function TTracker.CurrentPath: String;
begin
  Result := FCurrentPath;
end;

function TTracker.FindRoute(const AArgs: TRouteParam): TRouteAbstract;
var
  LKey: TRouteKey;
  LEndPoint: String;
  LRoute: TRouteAbstract;
begin
  // Request atualizada a cada requisi��o, para ser usada internamente
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

function TTracker.GetBind<T>(const ATag: String): T;
begin
  Result := FAppInjector^.Get<T>(ATag);
end;

function TTracker.GetBindInterface<I>(const ATag: String): I;
begin
  Result := FAppInjector^.GetInterface<I>(ATag);
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
  const AInjector: TAppInjector);
var
  LInstance: TModuleAbstract;
begin
  LInstance := _CreateModule(AModule);
  if LInstance = nil then
    Exit;
  try
    _AddModuleImportsBind(LInstance, AInjector);
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





