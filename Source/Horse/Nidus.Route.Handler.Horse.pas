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

unit Nidus.Route.Handler.Horse;

interface

uses
  Rtti,
  SysUtils,
  Horse,
  Horse.Callback,
  Nidus.Route.Handler;

type
  TRouteHandlerHorse = class(TRouteHandler)
  public
    function RouteGet(const ARoute: String; const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse; reintroduce;
    function RoutePost(const ARoute: String; const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse; reintroduce;
    function RoutePut(const ARoute: String; const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse; reintroduce;
    function RoutePatch(const ARoute: String; const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse; reintroduce;
    function RouteDelete(const ARoute: String; const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse; reintroduce;
  end;

implementation

{ TNFeRouteHandlerHorse }

function TRouteHandlerHorse.RouteDelete(const ARoute: String;
  const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
begin
  inherited RouteDelete(ARoute);
  THorse.Delete(ARoute, ACallback);
  Result := Self;
end;

function TRouteHandlerHorse.RouteGet(const ARoute: String;
  const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
begin
  inherited RouteGet(ARoute);
  THorse.Get(ARoute, ACallback);
  Result := Self;
end;

function TRouteHandlerHorse.RoutePatch(const ARoute: String;
  const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
begin
  inherited RoutePatch(ARoute);
  THorse.Patch(ARoute, ACallback);
  Result := Self;
end;

function TRouteHandlerHorse.RoutePost(const ARoute: String;
  const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
begin
  inherited RoutePost(ARoute);
  THorse.Post(ARoute, ACallback);
  Result := Self;
end;

function TRouteHandlerHorse.RoutePut(const ARoute: String;
  const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
begin
  inherited RoutePut(ARoute);
  THorse.Put(ARoute, ACallback);
  Result := Self;
end;

end.




