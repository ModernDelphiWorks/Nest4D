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

unit Nest4D.Route.Parse;

interface

uses
  System.Rtti,
  System.Types,
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Evolution4D.ResultPair,
  Nest4D.Route.Abstract,
  Nest4D.Route.Param,
  Nest4D.Route.Service,
  Nest4D.Route.Manager,
  Nest4D.Request,
  Nest4D.Exception,
  Nest4D.Listener,
  Nest4D.Injector,
  Nest4D.Module;

type
  TReturnPair = TResultPair<TRouteAbstract, Exception>;
  TGuardCallback = TFunc<Boolean>;

  TRouteParse = class
  private
    FAppInjector: PAppInjector;
    FService: TRouteService;
    FRouteManager: TRouteManager;
    procedure _ResolveRoutes(const APath: String;
      const ACallback: TFunc<String, TReturnPair>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncludeRouteService(const AService: TRouteService);
    function SelectRoute(const APath: String;
      const AReq: IRouteRequest = nil): TReturnPair;
  end;

implementation

{ TRouteParse }

constructor TRouteParse.Create;
begin
  FAppInjector := GAppInjector;
  if not Assigned(FAppInjector) then
    raise EAppInjector.Create;
  FRouteManager := FAppInjector^.Get<TRouteManager>;
end;

destructor TRouteParse.Destroy;
begin
  FAppInjector := nil;
  FService.Free;
  inherited;
end;

procedure TRouteParse.IncludeRouteService(const AService: TRouteService);
begin
  FService := AService;
end;

function TRouteParse.SelectRoute(const APath: String;
  const AReq: IRouteRequest): TReturnPair;
var
  LArgs: TRouteParam;
  LPath: String;
  LRouteParts: String;
  LRouteResult: TReturnPair;
  LListener: TAppListener;
begin
  LPath := LowerCase(APath);
  if LPath = '' then
  begin
    Result.Failure(ERouteNotFoundException.CreateFmt('', [APath]));
    Exit;
  end;
  if FRouteManager.FindEndPoint(LPath) <> '' then
  begin
    LArgs := TRouteParam.Create(LPath, AReq);
    LRouteResult := FService.GetRoute(LArgs);
    LListener := FAppInjector^.Get<TAppListener>;
    if Assigned(LListener) then
      LListener.Execute(FormatListenerMessage(Format('[RoutesResolver] %s', [APath])));
  end
  else
  begin
    LRouteParts := '';
    // Resolve routes
    _ResolveRoutes(APath,
                   function (Route: String): TReturnPair
                   begin
                     LRouteParts := LRouteParts + '/' + Route;
                     LArgs := TRouteParam.Create(LRouteParts, AReq);
                     if Assigned(LListener) then
                       LListener.Execute(FormatListenerMessage(Format('[RoutesResolver] %s', [LRouteParts])));
                     // If the condition "LRouteResult.isFailure" indicates that
                     // the previous route was not found, it is necessary to release
                     // the "Exception" variable to ensure that only the last route
                     // in the loop assigns a value to "LRouteResult."
                     // This ensures that any previously encountered errors or
                     // failures do not interfere with the final result, allowing
                     // the last valid route to be correctly assigned and used.
//                     if LRouteResult.isFailure then
//                       LRouteResult.Dispose;
                     LRouteResult := FService.GetRoute(LArgs);
                     Result := LRouteResult;
                   end);
  end;
  Result := LRouteResult;
end;

procedure TRouteParse._ResolveRoutes(const APath: String;
  const ACallback: TFunc<String, TReturnPair>);
var
  LRoutes: TStringDynArray;
  LRoute: String;
  LResult: TReturnPair;
begin
  LRoutes := SplitString(APath, '/');
  for LRoute in LRoutes do
  begin
    if (LRoute = '') or (LRoute = '/') then
      Continue;
    if LRoute = LRoutes[High(LRoutes)] then
      Break;
    LResult := ACallback(LRoute);
  end;
end;

end.










