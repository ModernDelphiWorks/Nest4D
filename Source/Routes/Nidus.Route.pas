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

unit Nidus.Route;

interface

uses
  Nidus.Route.Abstract;

type
  TRoute = class(TRouteAbstract)
  end;

  TRouteModule = class(TRoute)
  public
    class function AddModule(const APath: string;
      const AModule: TClass;
      const AMiddlewares: TMiddlewares): TRouteAbstract; override;
  end;

  TRouteChild = class(TRoute)
  public
    class function AddModule(const APath: string;
      const AModule: TClass;
      const AMiddlewares: TMiddlewares): TRouteAbstract; override;
  end;

implementation

{ TRouteModule }

class function TRouteModule.AddModule(const APath: string;
  const AModule: TClass;
  const AMiddlewares: TMiddlewares): TRouteAbstract;
begin
  inherited;
  Result := TRouteModule.Create(APath,
                                AModule.ClassName,
                                AModule,
                                AMiddlewares);
end;

{ TRouteChild }

class function TRouteChild.AddModule(const APath: string;
  const AModule: TClass;
  const AMiddlewares: TMiddlewares): TRouteAbstract;
begin
  inherited;
  Result := TRouteChild.Create(APath, '', AModule, AMiddlewares);
end;

end.

