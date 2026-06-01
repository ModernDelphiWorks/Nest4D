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

unit Nidus.Module;

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  Generics.Collections,
  Inject.Events,
  Nidus.Module.Abstract,
  Nidus.Route.Abstract,
  Nidus.Module.Service,
  Nidus.Route.Manager,
  Nidus.Route,
  Nidus.Route.Handler,
  Nidus.Bind,
  Nidus.Inject,
  Nidus.Request,
  Nidus.Listener;

type
  TValue = Rtti.TValue;
  TRouteMiddleware = Nidus.Route.Abstract.TRouteMiddleware;
  TRoute = Nidus.Route.TRoute;
  TRouteAbstract = Nidus.Route.Abstract.TRouteAbstract;
  TRoutes = Nidus.Module.Abstract.TRoutes;
  TBinds = Nidus.Module.Abstract.TBinds;
  TImports = Nidus.Module.Abstract.TImports;
  TExportedBinds = Nidus.Module.Abstract.TExportedBinds;
  TConstructorParams = Inject.Events.TConstructorParams;
  TRouteHandlers = Nidus.Module.Abstract.TRouteHandlers;
  TRouteManager = Nidus.Route.Manager.TRouteManager;
  IRouteRequest = Nidus.Request.IRouteRequest;

  TModule = class;

  TModule = class(TModuleAbstract)
  private
    FNidusInject: PNidusInject;
    FRouteHandlers: TObjectList<TRouteHandler>;
    FService: TModuleService;
    procedure _DestroyRoutes;
    procedure _DestroyInjector;
    procedure _AddRoutes;
    procedure _BindModule;
    procedure _RouteHandlers;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Routes: TRoutes; override;
    function Binds: TBinds; override;
    function Imports: TImports; override;
    function ExportedBinds: TExportedBinds; override;
    function RouteHandlers: TRouteHandlers; override;
  end;

  // Só para facilitar a sintaxe nos módulos
  Bind<T: class, constructor> = class(TBind<T>)
  end;

// RouteModule
function RouteModule(const APath: string;
  const AModule: TModuleClass): TRouteModule; overload;
function RouteModule(const APath: string; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares): TRouteModule; overload;

// RouteChild
function RouteChild(const APath: string; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares = []): TRouteChild;

implementation

uses
  ModernSyntax.Objects,
  Nidus.Exception;

function RouteModule(const APath: string; const AModule: TModuleClass): TRouteModule;
begin
  Result := nil;
  if Assigned(AModule) then
    Result := TRouteModule.AddModule(APath, AModule, nil{, []}) as TRouteModule;
end;

function RouteModule(const APath: string; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares): TRouteModule;
begin
  Result := nil;
  if Assigned(AModule) then
    Result := TRouteModule.AddModule(APath,
                                     AModule,
                                     AMiddlewares) as TRouteModule;
end;

function RouteChild(const APath: string; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares): TRouteChild;
begin
  Result := TRouteChild.AddModule(APath,
                                  AModule,
                                  AMiddlewares) as TRouteChild;
end;

{ TModuleAbstract }

constructor TModule.Create;
begin
  FNidusInject := GNidusInject;
  if not Assigned(FNidusInject) then
    raise EAppInjector.Create;
  // Service inject
  FService := FNidusInject^.Get<TModuleService>;
  // Routehandler list
  FRouteHandlers := TObjectList<TRouteHandler>.Create;
  // Load binds
  _BindModule;
  // Load routes
  _AddRoutes;
  // Load routehandles
  _RouteHandlers;
end;

destructor TModule.Destroy;
begin
  FNidusInject := nil;
  // Destroy as rotas do modulo
  _DestroyRoutes;
  // Destroy o injector do modulo
  _DestroyInjector;
  // Libera o servi?o
  FService.Free;
  // Libera os routehendlers
  FRouteHandlers.Free;
  // Console delphi
  inherited;
end;

function TModule.Binds: TBinds;
begin
  Result := [];
end;

function TModule.ExportedBinds: TExportedBinds;
begin
  Result := [];
end;

function TModule.Imports: TImports;
begin
  Result := [];
end;

function TModule.RouteHandlers: TRouteHandlers;
begin
  Result := [];
end;

function TModule.Routes: TRoutes;
begin
  Result := [];
end;

procedure TModule._BindModule;
begin
  FService.BindModule(Self)
end;

procedure TModule._AddRoutes;
begin
  FService.AddRoutes(Self);
end;

procedure TModule._DestroyInjector;
begin
  FService.ExtractInject<TNidusInject>(Self.ClassName);
end;

procedure TModule._DestroyRoutes;
begin
  FService.RemoveRoutes(Self.ClassName);
end;

procedure TModule._RouteHandlers;
var
  LHandler: TClass;
  LObject: TModernObject;
begin
  LObject := FNidusInject^.Get<TModernObject>;
  if not Assigned(LObject) then
    Exit;
  for LHandler in RouteHandlers do
    FRouteHandlers.Add(TRouteHandler(LObject.Factory(LHandler)));
end;

end.
