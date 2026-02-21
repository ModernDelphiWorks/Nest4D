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

unit Nest.Module.Provider;

interface

uses
  SysUtils,
  Evolution.ResultPair,
  Nest.Exception,
  Nest.Tracker,
  Nest.Route.Param,
  Nest.Module.Abstract,
  Nest.Route.Abstract,
  Nest.Listener,
  Nest.Injector;

type
  TModuleProvider = class
  private
    FAppInjector: PAppInjector;
    FTracker: TTracker;
    FListener: TAppListener;
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
end;

destructor TModuleProvider.Destroy;
begin
  FAppInjector := nil;
  FTracker := nil;
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
      LError := Format('Nest4d route (%s) not found!', [APath]);
      Result.Failure(LError);
      Exit;
    end;
    LModuleName := LRoute.ModuleInstance.ClassName;
    // Shouldn't change to .Free, as it also needs to receive Nil.
    FreeAndNil(LRoute.ModuleInstance);
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






