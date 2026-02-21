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

unit Nest.Module;

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  Generics.Collections,
  Injector.Events,
  Nest.Module.Abstract,
  Nest.Route.Abstract,
  Nest.Module.Service,
  Nest.Route.Manager,
  Nest.Route,
  Nest.Route.Handler,
  Nest.Bind,
  Nest.Injector,
  Nest.Request,
  Nest.Listener;

type
  TValue = Rtti.TValue;
  TRouteMiddleware = Nest.route.abstract.TRouteMiddleware;
  TRoute = Nest.route.TRoute;
  TRouteAbstract = Nest.route.abstract.TRouteAbstract;
  TRoutes = Nest.module.abstract.TRoutes;
  TBinds = Nest.module.abstract.TBinds;
  TImports = Nest.module.abstract.TImports;
  TExportedBinds = Nest.module.abstract.TExportedBinds;
  TConstructorParams = injector4d.events.TConstructorParams;
  TRouteHandlers = Nest.module.abstract.TRouteHandlers;
  TRouteManager = Nest.route.manager.TRouteManager;
  IRouteRequest = Nest.request.IRouteRequest;

  TModule = class;

  TModule = class(TModuleAbstract)
  private
    FAppInjector: PAppInjector;
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

  // S? para facilitar a sintaxe nos m?dulos
  Bind<T: class, constructor> = class(TBind<T>)
  end;

// RouteModule
function RouteModule(const APath: String;
  const AModule: TModuleClass): TRouteModule; overload;
function RouteModule(const APath: String; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares): TRouteModule; overload;

// RouteChild
function RouteChild(const APath: String; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares = []): TRouteChild;

implementation

uses
  System.Evolution.Objects,
  Nest.exception;

function RouteModule(const APath: String; const AModule: TModuleClass): TRouteModule;
begin
  Result := nil;
  if Assigned(AModule) then
    Result := TRouteModule.AddModule(APath, AModule, nil{, []}) as TRouteModule;
end;

function RouteModule(const APath: String; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares): TRouteModule;
begin
  Result := nil;
  if Assigned(AModule) then
    Result := TRouteModule.AddModule(APath,
                                     AModule,
                                     AMiddlewares) as TRouteModule;
end;

function RouteChild(const APath: String; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares): TRouteChild;
begin
  Result := TRouteChild.AddModule(APath,
                                  AModule,
                                  AMiddlewares) as TRouteChild;
end;

{ TModuleAbstract }

constructor TModule.Create;
begin
  FAppInjector := GAppInjector;
  if not Assigned(FAppInjector) then
    raise EAppInjector.Create;
  // Service inject
  FService := FAppInjector^.Get<TModuleService>;
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
  FAppInjector := nil;
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
  FService.ExtractInjector<TAppInjector>(Self.ClassName);
end;

procedure TModule._DestroyRoutes;
begin
  FService.RemoveRoutes(Self.ClassName);
end;

procedure TModule._RouteHandlers;
var
  LHandler: TClass;
  LObjectEx: TEvolutionObject;
begin
  LObjectEx := FAppInjector^.Get<TEvolutionObject>;
  if not Assigned(LObjectEx) then
    Exit;
  for LHandler in RouteHandlers do
    FRouteHandlers.Add(TRouteHandler(LObjectEx.Factory(LHandler)));
end;

end.







