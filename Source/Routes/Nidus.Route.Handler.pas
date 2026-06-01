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

unit Nidus.Route.Handler;

interface

uses
  Rtti,
  SysUtils,
  Nidus.Inject;

type
  TRouteHandler = class abstract
  private
    FNidusInject: PNidusInject;
    procedure _RegisterRouteHandle(const ARoute: string);
  protected
    procedure RegisterRoutes; virtual; abstract;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
    function RouteGet(const ARoute: string): TRouteHandler; virtual;
    function RoutePost(const ARoute: string): TRouteHandler; virtual;
    function RoutePut(const ARoute: string): TRouteHandler; virtual;
    function RouteDelete(const ARoute: string): TRouteHandler; virtual;
    function RoutePatch(const ARoute: string): TRouteHandler; virtual;
  end;

  TRouteHandlerClass = class of TRouteHandler;

implementation

uses
  Nidus.Register,
  Nidus.Exception;

constructor TRouteHandler.Create;
begin
  FNidusInject := GNidusInject;
  if not Assigned(FNidusInject) then
    raise EAppInjector.Create;
  RegisterRoutes;
end;

destructor TRouteHandler.Destroy;
begin
  FNidusInject := nil;
  inherited;
end;

function TRouteHandler.RouteDelete(const ARoute: string): TRouteHandler;
begin
  Result := Self;
  _RegisterRouteHandle(ARoute);
end;

function TRouteHandler.RouteGet(const ARoute: string): TRouteHandler;
begin
  Result := Self;
  _RegisterRouteHandle(ARoute);
end;

function TRouteHandler.RoutePatch(const ARoute: string): TRouteHandler;
begin
  Result := Self;
  _RegisterRouteHandle(ARoute);
end;

function TRouteHandler.RoutePost(const ARoute: string): TRouteHandler;
begin
  Result := Self;
  _RegisterRouteHandle(ARoute);
end;

function TRouteHandler.RoutePut(const ARoute: string): TRouteHandler;
begin
  Result := Self;
  _RegisterRouteHandle(ARoute);
end;

procedure TRouteHandler._RegisterRouteHandle(const ARoute: string);
var
  LRegister: TRegister;
begin
  LRegister := FNidusInject^.Get<TRegister>;
  if LRegister = nil then
    Exit;
  if not LRegister.ResgisterContainsKey(Self.ClassName) then
    Exit;
  if LRegister.RouteContainsKey(ARoute) then
    Exit;
  LRegister.Add(ARoute, Self.ClassName);
end;

end.


