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

unit Nidus.Module.Service;

interface

uses
  SysUtils,
  ModernSyntax.ResultPair,
  Nidus.Exception,
  Nidus.Route.Abstract,
  Nidus.Module.Abstract,
  Nidus.Module.Provider;

type
  TModuleService = class
  private
    FProvider: TModuleProvider;
  public
    destructor Destroy; override;
    procedure IncludeProvider(const AProvider: TModuleProvider);
    procedure Start(const AModule: TModuleAbstract;
      const AInitialRoutePath: string);
    procedure DisposeModule(const APath: string);
    procedure AddRoutes(const AModule: TModuleAbstract);
    procedure BindModule(const AModule: TModuleAbstract);
    procedure RemoveRoutes(const AModuleName: string);
    procedure ExtractInject<T: class>(const ATag: string);
  end;

implementation

{ TModuleService }

procedure TModuleService.DisposeModule(
  const APath: string);
var
  LResult: TResultPair<Boolean, string>;
begin
  LResult := FProvider.DisposeModule(APath);
end;

procedure TModuleService.ExtractInject<T>(const ATag: string);
begin
  FProvider.ExtractInject<T>(ATag);
end;

procedure TModuleService.AddRoutes(const AModule: TModuleAbstract);
begin
  FProvider.AddRoutes(AModule);
end;

procedure TModuleService.BindModule(const AModule: TModuleAbstract);
begin
  FProvider.BindModule(AModule);
end;

destructor TModuleService.Destroy;
begin
  if Assigned(FProvider) then
    FProvider.Free;
  inherited;
end;

procedure TModuleService.IncludeProvider(const AProvider: TModuleProvider);
begin
  FProvider := AProvider;
end;

procedure TModuleService.RemoveRoutes(const AModuleName: string);
begin
  FProvider.RemoveRoutes(AModuleName);
end;

procedure TModuleService.Start(const AModule: TModuleAbstract;
  const AInitialRoutePath: string);
var
  LResult: TResultPair<Boolean, string>;
begin
  LResult := FProvider.Start(AModule, AInitialRoutePath);
end;

end.

