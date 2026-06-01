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

unit Nidus.Module.Abstract;

interface

uses
  Generics.Collections,
  Nidus.Route,
  Nidus.Route.Handler,
  Nidus.Bind;

type
  TModuleAbstract = class;
  TModuleClass = class of TModuleAbstract;

  TRoutes = array of TRoute;
  TBinds = array of TBind<TObject>;
  TImports = array of TModuleClass;
  TExportedBinds = array of TBind<TObject>;
  TRouteHandlers = array of TRouteHandlerClass;

  TModuleAbstract = class
  public
    constructor Create; virtual; abstract;
    function Routes: TRoutes; virtual; abstract;
    function Binds: TBinds; virtual; abstract;
    function Imports: TImports; virtual; abstract;
    function ExportedBinds: TExportedBinds; virtual; abstract;
    function RouteHandlers: TRouteHandlers; virtual; abstract;
  end;

implementation

end.






