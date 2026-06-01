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

unit Nidus.Module.Provider;

interface

uses
  SysUtils,
  ModernSyntax.ResultPair,
  Nidus.Exception,
  Nidus.Tracker,
  Nidus.Route.Param,
  Nidus.Module.Abstract,
  Nidus.Route.Abstract,
  Nidus.Listener,
  Nidus.Inject,
  Nidus.Module.Cache.Interfaces;

type
  TModuleProvider = class
  private
    FNidusInject: PNidusInject;
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
    procedure ExtractInject<T: class>(const ATag: String);
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
  FNidusInject := GNidusInject;
  if not Assigned(FNidusInject) then
    raise EAppInjector.Create;
end;

destructor TModuleProvider.Destroy;
begin
  FNidusInject := nil;
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
  const AInitialRoutePath: string): TResultPair<Boolean, string>;
begin
  Result.Success(False);
  try
    FTracker.RunApp(AModule, AInitialRoutePath);
    FListener := FNidusInject^.Get<TAppListener>;
    Result.Success(True);
  except
    on E: Exception do
      Result.Failure(E.Message);
  end;
end;

function TModuleProvider.DisposeModule(const APath: string): TResultPair<Boolean, string>;
var
  LRoute: TRouteAbstract;
  LError: string;
  LModuleName: string;
  LCache: IModuleCache;
begin
  Result.Success(False);
  try
//    LRoute := FTracker.DisposeModule(TRouteParam.Create(APath));
    LRoute := FTracker.FindRoute(TRouteParam.Create(APath));
    if LRoute = nil then
    begin
      LError := Format('Nidus route (%s) not found!', [APath]);
      Result.Failure(LError);
      Exit;
    end;
    if not Assigned(LRoute.ModuleInstance) then
    begin
      Result.Success(True);
      Exit;
    end;
    LModuleName := LRoute.ModuleInstance.ClassName;
    LCache := GetGlobalModuleCache;
    if (not Assigned(LCache)) or (not LCache.IsEnabledFor(LRoute.Module)) then
      FreeAndNil(LRoute.ModuleInstance);
    if Assigned(FListener) then
      FListener.Execute(FormatListenerMessage(Format('[InstanceLoader] %s dependencies finalized', [LModuleName])));
    Result.Success(True);
  except
    on E: Exception do
      Result.Failure(E.Message);
  end;
end;

procedure TModuleProvider.ExtractInject<T>(const ATag: String);
begin
  FTracker.ExtractInject<T>(ATag);
end;

end.


