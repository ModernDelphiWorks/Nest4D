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
  Classes,
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

{ TRouteHandlerHorse }

// THorse is a process-global router and Nidus never removes routes from it on
// module/handler teardown (TRouteHandler.Destroy does not unregister). Recent
// Horse raises "Duplicate route detected" when the same [method] path handler
// is registered twice. A handler can be constructed more than once across an
// app/test lifecycle (diamond imports, per-test module builds), so guard the
// THorse registration to be idempotent — first registration wins, exactly the
// dedup the Nidus TRegister already applies to its own internal tracking.
var
  GRegisteredHorseRoutes: TStringList;

function _ShouldRegister(const AKey: string): Boolean;
begin
  Result := GRegisteredHorseRoutes.IndexOf(AKey) < 0;
  if Result then
    GRegisteredHorseRoutes.Add(AKey);
end;

function TRouteHandlerHorse.RouteDelete(const ARoute: String;
  const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
begin
  inherited RouteDelete(ARoute);
  if _ShouldRegister('DELETE ' + ARoute) then
    THorse.Delete(ARoute, ACallback);
  Result := Self;
end;

function TRouteHandlerHorse.RouteGet(const ARoute: String;
  const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
begin
  inherited RouteGet(ARoute);
  if _ShouldRegister('GET ' + ARoute) then
    THorse.Get(ARoute, ACallback);
  Result := Self;
end;

function TRouteHandlerHorse.RoutePatch(const ARoute: String;
  const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
begin
  inherited RoutePatch(ARoute);
  if _ShouldRegister('PATCH ' + ARoute) then
    THorse.Patch(ARoute, ACallback);
  Result := Self;
end;

function TRouteHandlerHorse.RoutePost(const ARoute: String;
  const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
begin
  inherited RoutePost(ARoute);
  if _ShouldRegister('POST ' + ARoute) then
    THorse.Post(ARoute, ACallback);
  Result := Self;
end;

function TRouteHandlerHorse.RoutePut(const ARoute: String;
  const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
begin
  inherited RoutePut(ARoute);
  if _ShouldRegister('PUT ' + ARoute) then
    THorse.Put(ARoute, ACallback);
  Result := Self;
end;

initialization
  GRegisteredHorseRoutes := TStringList.Create;
  GRegisteredHorseRoutes.Sorted := True;
  GRegisteredHorseRoutes.Duplicates := dupIgnore;

finalization
  GRegisteredHorseRoutes.Free;

end.
