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

var
  // DEC-050 — set True by TTracker._CreateModule only while constructing the
  // THROWAWAY instance it makes solely to HARVEST a module's ExportedBinds (the
  // Imports path). A module built in this mode must NOT register routes/injector on
  // Create, nor tear them down on Destroy: those belong to the module's REAL
  // instance (owned by its route). Without this, freeing the harvest instance ran
  // _DestroyRoutes/_DestroyInjector against the shared registration and destroyed an
  // IMPORTED route-bearing module's routes (the /api/v1/bancos-after-clientes 400).
  GNidusHarvesting: Boolean = False;

implementation

end.






