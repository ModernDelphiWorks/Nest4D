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

unit Nest4D.Route.Handler;

interface

uses
  System.Rtti,
  System.SysUtils,
  Nest4D.Injector;

type
  TRouteHandler = class abstract
  private
    FAppInjector: PAppInjector;
    procedure _RegisterRouteHandle(const ARoute: String);
  protected
    procedure RegisterRoutes; virtual; abstract;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
    function RouteGet(const ARoute: String): TRouteHandler; virtual;
    function RoutePost(const ARoute: String): TRouteHandler; virtual;
    function RoutePut(const ARoute: String): TRouteHandler; virtual;
    function RouteDelete(const ARoute: String): TRouteHandler; virtual;
    function RoutePatch(const ARoute: String): TRouteHandler; virtual;
  end;

  TRouteHandlerClass = class of TRouteHandler;

implementation

uses
  Nest4D.register,
  Nest4D.exception;

constructor TRouteHandler.Create;
begin
  FAppInjector := GAppInjector;
  if not Assigned(FAppInjector) then
    raise EAppInjector.Create;
  RegisterRoutes;
end;

destructor TRouteHandler.Destroy;
begin
  FAppInjector := nil;
  inherited;
end;

function TRouteHandler.RouteDelete(const ARoute: String): TRouteHandler;
begin
  Result := Self;
  _RegisterRouteHandle(ARoute);
end;

function TRouteHandler.RouteGet(const ARoute: String): TRouteHandler;
begin
  Result := Self;
  _RegisterRouteHandle(ARoute);
end;

function TRouteHandler.RoutePatch(const ARoute: String): TRouteHandler;
begin
  Result := Self;
  _RegisterRouteHandle(ARoute);
end;

function TRouteHandler.RoutePost(const ARoute: String): TRouteHandler;
begin
  Result := Self;
  _RegisterRouteHandle(ARoute);
end;

function TRouteHandler.RoutePut(const ARoute: String): TRouteHandler;
begin
  Result := Self;
  _RegisterRouteHandle(ARoute);
end;

procedure TRouteHandler._RegisterRouteHandle(const ARoute: String);
var
  LRegister: TRegister;
begin
  LRegister := FAppInjector^.Get<TRegister>;
  if LRegister = nil then
    Exit;
  if not LRegister.ResgisterContainsKey(Self.ClassName) then
    Exit;
  if LRegister.RouteContainsKey(ARoute) then
    Exit;
  LRegister.Add(ARoute, Self.ClassName);
end;

end.








