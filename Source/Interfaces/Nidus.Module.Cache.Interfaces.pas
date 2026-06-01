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

unit Nidus.Module.Cache.Interfaces;

interface

uses
  SysUtils;

type
  TNidusCacheAll = class
  end;

  IModuleCache = interface
    ['{2B9D7C43-7D03-4F3F-9F5D-4C47F6B15F69}']
    function ResolveInstance(const AModuleClass: TClass; const AFactory: TFunc<TObject>): TObject;
    procedure Invalidate(const AModuleClass: TClass);
    procedure Clear;
    function SetPolicy(const AModuleClasses: array of TClass): IModuleCache;
    function EnableAll: IModuleCache;
    function DisableAll: IModuleCache;
    function IsEnabledFor(const AModuleClass: TClass): Boolean;
  end;

procedure SetGlobalModuleCache(const ACache: IModuleCache);
function GetGlobalModuleCache: IModuleCache;

implementation

var
  GModuleCache: IModuleCache;

procedure SetGlobalModuleCache(const ACache: IModuleCache);
begin
  GModuleCache := ACache;
end;

function GetGlobalModuleCache: IModuleCache;
begin
  Result := GModuleCache;
end;

end.

