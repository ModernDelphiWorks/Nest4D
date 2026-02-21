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

unit Nest4D.Module;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections,
  Injector4D.Events,
  Nest4D.Module.Abstract,
  Nest4D.Route.Abstract,
  Nest4D.Module.Service,
  Nest4D.Route.Manager,
  Nest4D.Route,
  Nest4D.Route.Handler,
  Nest4D.Bind,
  Nest4D.Injector,
  Nest4D.Request,
  Nest4D.Listener;

type
  TValue = System.Rtti.TValue;
  TRouteMiddleware = Nest4D.Route.Abstract.TRouteMiddleware;
  TRoute = Nest4D.Route.TRoute;
  TRouteAbstract = Nest4D.Route.Abstract.TRouteAbstract;
  TRoutes = Nest4D.Module.Abstract.TRoutes;
  TBinds = Nest4D.Module.Abstract.TBinds;
  TImports = Nest4D.Module.Abstract.TImports;
  TExportedBinds = Nest4D.Module.Abstract.TExportedBinds;
  TConstructorParams = Injector4D.Events.TConstructorParams;
  TRouteHandlers = Nest4D.Module.Abstract.TRouteHandlers;
  TRouteManager = Nest4D.Route.Manager.TRouteManager;
  IRouteRequest = Nest4D.Request.IRouteRequest;

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
function RouteModule(const APath: String; const AModule: TModuleClass;
  const AUsePool: Boolean): TRouteModule; overload;
function RouteModule(const APath: String; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares; const AUsePool: Boolean): TRouteModule; overload;

// RouteChild
function RouteChild(const APath: String; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares = []): TRouteChild;

implementation

uses
  Evolution4D.Objects,
  Nest4D.Exception;

function RouteModule(const APath: String; const AModule: TModuleClass): TRouteModule;
begin
  Result := nil;
  if Assigned(AModule) then
    Result := TRouteModule.AddModule(APath, AModule, nil{, []}, False) as TRouteModule;
end;

function RouteModule(const APath: String; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares): TRouteModule;
begin
  Result := nil;
  if Assigned(AModule) then
    Result := TRouteModule.AddModule(APath,
                                     AModule,
                                     AMiddlewares,
                                     False) as TRouteModule;
end;

function RouteModule(const APath: String; const AModule: TModuleClass;
  const AUsePool: Boolean): TRouteModule;
begin
  Result := nil;
  if Assigned(AModule) then
    Result := TRouteModule.AddModule(APath, AModule, nil, AUsePool) as TRouteModule;
end;

function RouteModule(const APath: String; const AModule: TModuleClass;
  const AMiddlewares: TMiddlewares; const AUsePool: Boolean): TRouteModule;
begin
  Result := nil;
  if Assigned(AModule) then
    Result := TRouteModule.AddModule(APath,
                                     AModule,
                                     AMiddlewares,
                                     AUsePool) as TRouteModule;
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
  LObject: TEvolutionObject;
begin
  LObject := FAppInjector^.Get<TEvolutionObject>;
  if not Assigned(LObject) then
    Exit;
  for LHandler in RouteHandlers do
    FRouteHandlers.Add(TRouteHandler(LObject.Factory(LHandler)));
end;

end.








