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

unit Nidus.Route.Service;

interface

uses
  Rtti,
  Classes,
  SysUtils,
  Nidus.Route.Provider,
  Nidus.Route.Param,
  Nidus.Route.Abstract,
  Nidus.Exception,
  ModernSyntax.ResultPair;

type
  TRouteService = class
  private
    FProvider: TRouteProvider;
  public
    destructor Destroy; override;
    procedure IncludeProvider(const AProvider: TRouteProvider);
    function GetRoute(const AArgs: TRouteParam): TResultPair<TRouteAbstract, Exception>;
  end;

implementation

{ TRouteService }

destructor TRouteService.Destroy;
begin
  FProvider.Free;
  inherited;
end;

function TRouteService.GetRoute(const AArgs: TRouteParam): TResultPair<TRouteAbstract, Exception>;
begin
  try
    Result := FProvider.GetRoute(AArgs);
    if Result.ValueSuccess = nil then
      Result.Failure(ERouteNotFoundException.CreateFmt('', [AArgs.Path]));
  except
    on E: EUnauthorizedException do
      Result.Failure(EUnauthorizedException.Create(''));
    on E: Exception do
      Result.Failure(EBadRequestException.Create(E.Message));
  end;
end;

procedure TRouteService.IncludeProvider(const AProvider: TRouteProvider);
begin
  FProvider := AProvider;
end;

end.








