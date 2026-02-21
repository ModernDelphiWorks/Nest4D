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

unit Nest4D.Module.Provider;

interface

uses
  System.SysUtils,
  Evolution4D.ResultPair,
  Nest4D.Exception,
  Nest4D.Tracker,
  Nest4D.Route.Param,
  Nest4D.Module.Abstract,
  Nest4D.Route.Abstract,
  Nest4D.Listener,
  Nest4D.Injector,
  Nest4D.Pool.Config;

type
  TModuleProvider = class
  private
    FAppInjector: PAppInjector;
    FTracker: TTracker;
    FListener: TAppListener;
    FPoolConfig: TPoolConfig;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncludeTracker(const ATracker: TTracker);
    function Start(const AModule: TModuleAbstract;
      const AInitialRoutePath: String): TResultPair<Boolean, String>;
    function DisposeModule(const APath: String): TResultPair<Boolean, String>;
    procedure AddRoutes(const AModule: TModuleAbstract);
    procedure BindModule(const AModule: TModuleAbstract);
    procedure RemoveRoutes(const AModuleName: String);
    procedure ExtractInjector<T: class>(const ATag: String);
  end;

implementation

procedure TModuleProvider.AddRoutes(const AModule: TModuleAbstract);
begin
  FTracker.AddRoutes(AModule);
end;

procedure TModuleProvider.BindModule(const AModule: TModuleAbstract);
begin
  FTracker.BindModule(AModule);
end;

constructor TModuleProvider.Create;
begin
  FAppInjector := GAppInjector;
  if not Assigned(FAppInjector) then
    raise EAppInjector.Create;
  FPoolConfig := TPoolConfig.Create;
end;

destructor TModuleProvider.Destroy;
begin
  FAppInjector := nil;
  FTracker := nil;
  if Assigned(FPoolConfig) then
    FPoolConfig.Free;
  inherited;
end;

procedure TModuleProvider.IncludeTracker(
  const ATracker: TTracker);
begin
  FTracker := ATracker;
end;

procedure TModuleProvider.RemoveRoutes(const AModuleName: String);
begin
  FTracker.RemoveRoutes(AModuleName);
end;

function TModuleProvider.Start(const AModule: TModuleAbstract;
  const AInitialRoutePath: String): TResultPair<Boolean, String>;
begin
  try
    FTracker.RunApp(AModule, AInitialRoutePath);
    FListener := FAppInjector^.Get<TAppListener>;
    Result.Success(True);
  except
    on E: Exception do
      Result.Failure(E.Message);
  end;
end;

function TModuleProvider.DisposeModule(
  const APath: String): TResultPair<Boolean, String>;
var
  LRoute: TRouteAbstract;
  LError: String;
  LModuleName: String;
begin
  try
    LRoute := FTracker.DisposeModule(TRouteParam.Create(APath));
    if LRoute = nil then
    begin
      LError := Format('Nest4D route (%s) not found!', [APath]);
      Result.Failure(LError);
      Exit;
    end;
    LModuleName := LRoute.ModuleInstance.ClassName;
    
    // Verificar se o módulo usa pool
    if LRoute.UsePool and Assigned(FPoolConfig) then
    begin
      // Retornar módulo para o pool ao invés de destruir
      FPoolConfig.ReturnModule(LModuleName, TModuleAbstract(LRoute.ModuleInstance));
      LRoute.ModuleInstance := nil; // Limpar referência sem destruir
    end
    else
    begin
      // Shouldn't change to .Free, as it also needs to receive Nil.
      FreeAndNil(LRoute.ModuleInstance);
    end;
    
    if Assigned(FListener) then
      FListener.Execute(FormatListenerMessage(Format('[InstanceLoader] %s dependencies finalized', [LModuleName])));
    Result.Success(True);
  except
    on E: Exception do
      Result.Failure(E.Message);
  end;
end;

procedure TModuleProvider.ExtractInjector<T>(const ATag: String);
begin
  FTracker.ExtractInjector<T>(ATag);
end;

end.







