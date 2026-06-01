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

unit Nidus;

interface

uses
  Rtti,
  Types,
  Classes,
  SysUtils,
  StrUtils,
  Generics.Collections,
  Inject.Service,
  ModernSyntax.ResultPair,
  Nidus.Module.Cache.Interfaces,
  Nidus.Pooling.Interfaces,
  Nidus.Module,
  Nidus.Route.Parse,
  Nidus.Module.Service,
  Nidus.Bind.Service,
  Nidus.Validation.Interfaces,
  Nidus.Route.Handler,
  Nidus.RPC.Interfaces,
  Nidus.RPC.Resource,
  Nidus.Inject,
  Nidus.Exception,
  Nidus.Register,
  Nidus.Request,
  Nidus.Listener;

type
  TNidus = class sealed
  strict private
    class var FListener: TAppListener;
  private
    FNidusInject: PNidusInject;
    FInitialRoutePath: string;
    FAppModule: TModule;
    FRouteParse: TRouteParse;
    FModuleService: TModuleService;
    FBindService: TBindService;
    FModuleStarted: Boolean;
    FRequest: IRouteRequest;
    FGuardCallback: TGuardCallback;
    FRegister: TRegister;
    FRPCProviderServer: IRPCProviderServer;
    procedure _ResolveDisposeRouteModule(const APath: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncludeModuleService(const AService: TModuleService);
    procedure IncludeBindService(const AService: TBindService);
    procedure IncludeRouteParser(const ARouteParse: TRouteParse);
    function Start(const AModule: TModule;
      const AInitialRoutePath: string = '/'): TNidus;
    procedure Finalize;
    procedure DisposeRouteModule(const APath: string);
    procedure RegisterRouteHandler(const ARouteHandler: TRouteHandlerClass);
    procedure Listener(const AMessage: string);
    function UseGuard(const AGuardCallback: TGuardCallback): TNidus;
    function UsePipes(const AValidationPipe: IValidationPipe): TNidus;
    function UseCache(const ACache: IModuleCache): TNidus; overload;
    function UseCache(const ACache: IModuleCache; const AModules: array of TClass): TNidus; overload;
    function Cache: IModuleCache;
    function UsePools(const ARegistry: IPoolRegistry): TNidus; overload;
    function UsePools<T: class, constructor>(const AMaxSize: Integer = 256): TNidus; overload;
    function UsePools<T: TComponent>(const AMaxSize: Integer = 32;
      const AOwner: TComponent = nil; const AReset: TProc<T> = nil): TNidus; overload;
    function UseComponentPool<T: TComponent>(const AMaxSize: Integer = 32;
      const AOwner: TComponent = nil; const AReset: TProc<T> = nil): TNidus; overload;
    function Pools: IPoolRegistry;
    function UseRPC(const ARPCProviderServer: IRPCProviderServer): TNidus;
    class function UseListener(const AListener: TListener): TNidus;
    function PublishRPC(const ARPCName: string; const ARPCClass: TRPCResourceClass): TNidus;
    function LoadRouteModule(const APath: string;
      const AReq: IRouteRequest = nil): TReturnPair;
    function Get<T: class, constructor>(ATag: string = ''): T;
    function GetInterface<I: IInterface>(ATag: string = ''): I;
    function Request: IRouteRequest;
    procedure WithPool<T: class>(const AProc: TProc<T>); overload;
    procedure WithPool<T: class>(const AKey: string; const AProc: TProc<T>); overload;
  end;

function GetNidus: TNidus;

implementation

uses
  Nidus.Pooling.Registry,
  Nidus.Pooling.Helpers;

{ TNidus }

function GetNidus: TNidus;
begin
  Result := GNidusInject^.Get<TNidus>;
end;

constructor TNidus.Create;
begin
  FNidusInject := GNidusInject;
  if not Assigned(FNidusInject) then
    raise EAppInjector.Create;

  FModuleStarted := False;
  FRegister := FNidusInject^.Get<TRegister>;
end;

destructor TNidus.Destroy;
begin
  if Assigned(FBindService) then
    FBindService.Free;

  if Assigned(FModuleService) then
    FModuleService.Free;

  if Assigned(FRouteParse) then
    FRouteParse.Free;

  if Assigned(FRPCProviderServer) then
    FRPCProviderServer.Stop;

  FModuleStarted := False;
  FNidusInject := nil;
  FRegister := nil;
  inherited;
end;

function TNidus.GetInterface<I>(ATag: string = ''): I;
var
  LResult: TResultPair<I, Exception>;
begin
  LResult := FBindService.GetBindInterface<I>(ATag);
  LResult.When(
    procedure (AValue: I)
    begin

    end,
    procedure (AValue: Exception)
    begin
      raise AValue;
    end);
  Result := LResult.ValueSuccess;
end;

function TNidus.UseCache(const ACache: IModuleCache): TNidus;
begin
  SetGlobalModuleCache(ACache);
  Result := Self;
end;

function TNidus.UseCache(const ACache: IModuleCache; const AModules: array of TClass): TNidus;
begin
  UseCache(ACache);
  if Assigned(ACache) then
    ACache.SetPolicy(AModules);
  Result := Self;
end;

function TNidus.Cache: IModuleCache;
begin
  Result := GetGlobalModuleCache;
end;

function TNidus.UsePools(const ARegistry: IPoolRegistry): TNidus;
begin
  SetGlobalPoolRegistry(ARegistry);
  Result := Self;
end;

function TNidus.UsePools<T>(const AMaxSize: Integer): TNidus;
begin
  TPoolRegistry.RegisterDefaultObjectPool<T>(AMaxSize);
  Result := Self;
end;

function TNidus.UsePools<T>(const AMaxSize: Integer; const AOwner: TComponent;
  const AReset: TProc<T>): TNidus;
begin
  TPoolRegistry.RegisterDefaultComponentPool<T>(AMaxSize, AOwner, AReset);
  Result := Self;
end;

function TNidus.UseComponentPool<T>(const AMaxSize: Integer; const AOwner: TComponent;
  const AReset: TProc<T>): TNidus;
begin
  Result := UsePools<T>(AMaxSize, AOwner, AReset);
end;

function TNidus.Pools: IPoolRegistry;
begin
  Result := GetGlobalPoolRegistry;
end;

procedure TNidus.WithPool<T>(const AProc: TProc<T>);
begin
  TPoolHelpers.WithPool<T>(AProc);
end;

procedure TNidus.WithPool<T>(const AKey: string; const AProc: TProc<T>);
begin
  TPoolHelpers.WithPool<T>(AKey, AProc);
end;

function TNidus.Get<T>(ATag: string): T;
var
  LResult: TResultPair<T, Exception>;
begin
  LResult := FBindService.GetBind<T>(ATag);
  LResult.When(
    procedure (AValue: T)
    begin

    end,
    procedure (AValue: Exception)
    begin
      raise AValue;
    end);
  Result := LResult.ValueSuccess;
end;

procedure TNidus.IncludeBindService(const AService: TBindService);
begin
  FBindService := AService;
end;

procedure TNidus.IncludeModuleService(const AService: TModuleService);
begin
  FModuleService := AService;
end;

procedure TNidus.IncludeRouteParser(const ARouteParse: TRouteParse);
begin
  FRouteParse := ARouteParse;
end;

function TNidus.Start(const AModule: TModule;
  const AInitialRoutePath: string): TNidus;
var
  LModule: TClass;
begin
  if FModuleStarted then
    raise EModuleStartedException.CreateFmt('', [AModule.ClassName]);

  FInitialRoutePath := AInitialRoutePath;
  FAppModule := AModule;
  FModuleService.Start(AModule, AInitialRoutePath);
  FModuleStarted := True;
  Result := Self;
  if Assigned(FListener) then
  begin
    FListener.Execute(FormatListenerMessage('[NidusStart] Starting Nidus application...'));
    FListener.Execute(FormatListenerMessage('[InstanceLoader] AppModule dependencies initialized'));
    for LModule in AModule.Imports do
      FListener.Execute(FormatListenerMessage(Format('[InstanceImported] %s dependencies imported', [LModule.ClassName])));
  end;
end;

function TNidus.UseGuard(const AGuardCallback: TGuardCallback): TNidus;
begin
  FGuardCallback := AGuardCallback;
  Result := Self;
end;

class function TNidus.UseListener(const AListener: TListener): TNidus;
begin
  if not Assigned(AListener) then
    Exit;
  GNidusInject^.AddInstance<TAppListener>(TAppListener.Create(AListener));
  FListener := GNidusInject^.Get<TAppListener>;
end;

function TNidus.UsePipes(const AValidationPipe: IValidationPipe): TNidus;
begin
  FRegister.UsePipes(AValidationPipe);
  Result := Self;
end;

function TNidus.UseRPC(const ARPCProviderServer: IRPCProviderServer): TNidus;
begin
  FRPCProviderServer := ARPCProviderServer;
  FRPCProviderServer.Start;
  Result := Self;
end;

function TNidus.LoadRouteModule(const APath: String;
  const AReq: IRouteRequest): TReturnPair;
var
  LRouteHandle: TClass;
  LIsAccessGranted: Boolean;
begin
  FRequest := AReq;
  if Assigned(FGuardCallback) then
  begin
    LIsAccessGranted := FGuardCallback;
    if not LIsAccessGranted then
      raise EUnauthorizedException.Create('');
  end;
  if FRegister.IsValidationPipe then
  begin
    LRouteHandle := FRegister.FindRecord(APath);
    if LRouteHandle <> nil then
    begin
      FRegister.Pipe.Validate(LRouteHandle, FRequest);
      if FRegister.Pipe.IsMessages then
      begin
        Result.Failure(EBadRequestException.Create(FRegister.Pipe.BuildMessages));
        Exit;
      end;
//      Result.Failure(EBadRequestException.Create('Use the "UsePipes" command followed by "TValidationPipe.Create" to enable global validation pipes.'));
//      exit;
    end;
  end;
  Result := FRouteParse.SelectRoute(APath, AReq);
end;

procedure TNidus.Listener(const AMessage: String);
begin
  if Assigned(FListener) then
    FListener.Execute(AMessage);
end;

function TNidus.PublishRPC(const ARPCName: String;
  const ARPCClass: TRPCResourceClass): TNidus;
begin
  if not Assigned(FRPCProviderServer) then
    raise ERPCProviderNotSetException.Create;
  FRPCProviderServer.PublishRPC(ARPCName, ARPCClass);
  Result := Self;
end;

procedure TNidus.RegisterRouteHandler(const ARouteHandler: TRouteHandlerClass);
begin
  FRegister.Add(ARouteHandler);
end;

function TNidus.Request: IRouteRequest;
begin
  Result := FRequest;
end;

procedure TNidus.DisposeRouteModule(const APath: string);
begin
  _ResolveDisposeRouteModule(APath);
end;

procedure TNidus.Finalize;
begin
  // Do not change the order.
  FAppModule.Free;
  FNidusInject^.ExtractInject<TNidusInject>(C_NIDUS);
end;

procedure TNidus._ResolveDisposeRouteModule(const APath: string);
var
  LRoutes: TStringDynArray;
  LRoute: string;
  LRouteParts: string;
begin
  LRouteParts := '';
  LRoutes := SplitString(APath, '/');
  for LRoute in LRoutes do
  begin
    if (LRoute = '') or (LRoute = '/') then
      Continue;
    LRouteParts := LRouteParts + '/' + LRoute;
    FModuleService.DisposeModule(LRouteParts);
  end;
end;

end.
