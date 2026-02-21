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
  @abstract(Nest4D Framework for Delphi)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://nest4d-en.docs-br.com)
}

unit Nest;

interface

uses
  Rtti,
  Types,
  Classes,
  SysUtils,
  StrUtils,
  Generics.Collections,
  Injector.Service,
  Evolution.ResultPair,
  Nest.Module,
  Nest.Route.Parse,
  Nest.Module.Service,
  Nest.Bind.Service,
  Nest.Validation.Interfaces,
  Nest.Route.Handler,
  Nest.RPC.Interfaces,
  Nest.RPC.Resource,
  Nest.Injector,
  Nest.Exception,
  Nest.Register,
  Nest.Request,
  Nest.Listener;

type
  TNest4D = class sealed
  strict private
    class var FListener: TAppListener;
  private
    FAppInjector: PAppInjector;
    FInitialRoutePath: String;
    FAppModule: TModule;
    FRouteParse: TRouteParse;
    FModuleService: TModuleService;
    FBindService: TBindService;
    FModuleStarted: Boolean;
    FRequest: IRouteRequest;
    FGuardCallback: TGuardCallback;
    FRegister: TRegister;
    FRPCProviderServer: IRPCProviderServer;
    procedure _ResolveDisposeRouteModule(const APath: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncludeModuleService(const AService: TModuleService);
    procedure IncludeBindService(const AService: TBindService);
    procedure IncludeRouteParser(const ARouteParse: TRouteParse);
    function Start(const AModule: TModule;
      const AInitialRoutePath: String = '/'): TNest4D;
    procedure Finalize;
    procedure DisposeRouteModule(const APath: String);
    procedure RegisterRouteHandler(const ARouteHandler: TRouteHandlerClass);
    procedure Listener(const AMessage: String);
    function UseGuard(const AGuardCallback: TGuardCallback): TNest4D;
    function UsePipes(const AValidationPipe: IValidationPipe): TNest4D;
    function UseRPC(const ARPCProviderServer: IRPCProviderServer): TNest4D;
    class function UseListener(const AListener: TListener): TNest4D;
    function PublishRPC(const ARPCName: String; const ARPCClass: TRPCResourceClass): TNest4D;
    function LoadRouteModule(const APath: String;
      const AReq: IRouteRequest = nil): TReturnPair;
    function Get<T: class, constructor>(ATag: String = ''): T;
    function GetInterface<I: IInterface>(ATag: String = ''): I;
    function Request: IRouteRequest;
  end;

function GetNest4d: TNest4D;

implementation

{ TNest4D }

function GetNest4d: TNest4D;
begin
  Result := GAppInjector^.Get<TNest4D>;
end;

constructor TNest.Create;
begin
  FAppInjector := GAppInjector;
  if not Assigned(FAppInjector) then
    raise EAppInjector.Create;
  FModuleStarted := False;
  FRegister := FAppInjector^.Get<TRegister>;
end;

destructor TNest.Destroy;
begin
  FAppInjector := nil;
  if Assigned(FBindService) then
    FBindService.Free;
  if Assigned(FModuleService) then
    FModuleService.Free;
  if Assigned(FRouteParse) then
    FRouteParse.Free;
  FModuleStarted := False;
  if Assigned(FRPCProviderServer) then
    FRPCProviderServer.Stop;
  FRegister := nil;
  inherited;
end;

function TNest.GetInterface<I>(ATag: String = ''): I;
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

function TNest.Get<T>(ATag: String): T;
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

procedure TNest.IncludeBindService(const AService: TBindService);
begin
  FBindService := AService;
end;

procedure TNest.IncludeModuleService(const AService: TModuleService);
begin
  FModuleService := AService;
end;

procedure TNest.IncludeRouteParser(const ARouteParse: TRouteParse);
begin
  FRouteParse := ARouteParse;
end;

function TNest.Start(const AModule: TModule;
  const AInitialRoutePath: String): TNest4D;
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
    FListener.Execute(FormatListenerMessage('[Nest4dStart] Starting Nest4D application...'));
    FListener.Execute(FormatListenerMessage('[InstanceLoader] AppModule dependencies initialized'));
    for LModule in AModule.Imports do
      FListener.Execute(FormatListenerMessage(Format('[InstanceImported] %s dependencies imported', [LModule.ClassName])));
  end;
end;

function TNest.UseGuard(const AGuardCallback: TGuardCallback): TNest4D;
begin
  FGuardCallback := AGuardCallback;
  Result := Self;
end;

class function TNest.UseListener(const AListener: TListener): TNest4D;
begin
  if not Assigned(AListener) then
    Exit;
  GAppInjector^.AddInstance<TAppListener>(TAppListener.Create(AListener));
  FListener := GAppInjector^.Get<TAppListener>;
end;

function TNest.UsePipes(const AValidationPipe: IValidationPipe): TNest4D;
begin
  FRegister.UsePipes(AValidationPipe);
  Result := Self;
end;

function TNest.UseRPC(const ARPCProviderServer: IRPCProviderServer): TNest4D;
begin
  FRPCProviderServer := ARPCProviderServer;
  FRPCProviderServer.Start;
  Result := Self;
end;

function TNest.LoadRouteModule(const APath: String;
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

procedure TNest.Listener(const AMessage: String);
begin
  if Assigned(FListener) then
    FListener.Execute(AMessage);
end;

function TNest.PublishRPC(const ARPCName: String;
  const ARPCClass: TRPCResourceClass): TNest4D;
begin
  if not Assigned(FRPCProviderServer) then
    raise ERPCProviderNotSetException.Create;
  FRPCProviderServer.PublishRPC(ARPCName, ARPCClass);
  Result := Self;
end;

procedure TNest.RegisterRouteHandler(const ARouteHandler: TRouteHandlerClass);
begin
  FRegister.Add(ARouteHandler);
end;

function TNest.Request: IRouteRequest;
begin
  Result := FRequest;
end;

procedure TNest.DisposeRouteModule(const APath: String);
begin
  _ResolveDisposeRouteModule(APath);
end;

procedure TNest.Finalize;
begin
  // Do not change the order.
  FAppModule.Free;
  FAppInjector^.ExtractInjector<TAppInjector>(C_NEST4D);
end;

procedure TNest._ResolveDisposeRouteModule(const APath: String);
var
  LRoutes: TStringDynArray;
  LRoute: String;
  LRouteParts: String;
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





