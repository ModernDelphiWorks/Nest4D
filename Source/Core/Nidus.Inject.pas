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

unit Nidus.Inject;

interface

uses
  Rtti,
  SysUtils,
  Inject,
  Inject.Service,
  Generics.Collections;

type
  PNidusInject = ^TNidusInject;
  TNidusInject = class(TInject)
  public
    procedure CreateNidusdInject;
    procedure ExtractInject<T: class>(const ATag: string = '');
  end;

  TCoreInject = class(TNidusInject)
  private
    procedure _TrackeInjector; // Singleton
    procedure _ObjectFactoryInjector; // SingletonLazy
    procedure _BindServiceInjector; // Factory
    procedure _RouteServiceInjector; // Factory
    procedure _ModuleServiceInjector; // Factory
    procedure _ModuleProviderInjector; // Factory
    procedure _BindProviderInjector; // Factory
    procedure _RouteProviderInjector; // Factory
    procedure _RouteParseInjector; // Factory
    procedure _NidusInject; // SingletonLazy
  public
    constructor Create; override;
  end;

var
  GNidusInject: PNidusInject = nil;

const C_NIDUS = 'Nidus';

implementation

uses
  ModernSyntax.Objects,
  Nidus.Bind.Provider,
  Nidus.Bind.Service,
  Nidus.Module.Provider,
  Nidus.Module.Service,
  Nidus.Route.Provider,
  Nidus.Route.Parse,
  Nidus.Route.Service,
  Nidus.Route.Manager,
  Nidus.Tracker,
  Nidus.Register,
  Nidus;

{ TCoreInjector }

constructor TCoreInject.Create;
begin
  inherited;
  // Datasource
  _TrackeInjector;
  _ObjectFactoryInjector;
  // Infra
  _BindServiceInjector;
  _RouteServiceInjector;
  _ModuleServiceInjector;
  // Domain
  _ModuleProviderInjector;
  _BindProviderInjector;
  _RouteProviderInjector;
  _RouteParseInjector;
  _NidusInject;
end;

procedure TCoreInject._BindProviderInjector;
begin
  Self.Factory<TBindProvider>(nil, nil,
    function: TConstructorParams
    begin
      Result := [TValue.From<TTracker>(Self.Get<TTracker>)];
    end);
end;

procedure TCoreInject._BindServiceInjector;
begin
  Self.Factory<TBindService>(
    procedure(Value: TBindService)
    begin
      Value.IncludeBindProvider(Self.Get<TBindProvider>);
    end);
end;

procedure TCoreInject._NidusInject;
begin
  Self.SingletonLazy<TNidus>(
    procedure(Value: TNidus)
    begin
      Value.IncludeModuleService(Self.Get<TModuleService>);
      Value.IncludeBindService(Self.Get<TBindService>);
      Value.IncludeRouteParser(Self.Get<TRouteParse>);
    end);
end;

procedure TCoreInject._ModuleProviderInjector;
begin
  Self.Factory<TModuleProvider>(
    procedure(Value: TModuleProvider)
    begin
      Value.IncludeTracker(Self.Get<TTracker>);
    end);
end;

procedure TCoreInject._ModuleServiceInjector;
begin
  Self.Factory<TModuleService>(
    procedure(Value: TModuleService)
    begin
      Value.IncludeProvider(Self.Get<TModuleProvider>);
    end);
end;

procedure TCoreInject._RouteParseInjector;
begin
  Self.Factory<TRouteParse>(
    procedure(Value: TRouteParse)
    begin
      Value.IncludeRouteService(Self.Get<TRouteService>);
    end);
end;

procedure TCoreInject._RouteProviderInjector;
begin
  Self.Factory<TRouteProvider>(
    procedure(Value: TRouteProvider)
    begin
      Value.IncludeTracker(Self.Get<TTracker>);
    end);
end;

procedure TCoreInject._RouteServiceInjector;
begin
  Self.Factory<TRouteService>(
    procedure(Value: TRouteService)
    begin
      Value.IncludeProvider(Self.Get<TRouteProvider>);
    end);
end;

procedure TCoreInject._TrackeInjector;
begin
  Self.SingletonLazy<TTracker>;
  Self.SingletonLazy<TRouteManager>;
end;

procedure TCoreInject._ObjectFactoryInjector;
begin
  Self.Singleton<TModernObject>;
end;

procedure TNidusInject.CreateNidusdInject;
begin
  GNidusInject^.AddInject(C_NIDUS, TCoreInject.Create);
  GNidusInject^.AddInstance<TRegister>(TRegister.Create);
end;

procedure TNidusInject.ExtractInject<T>(const ATag: String);
var
  LKey: String;
begin
  LKey := ATag;
  if LKey = '' then
    LKey := T.ClassName;
  Self.Remove<T>(LKey);
end;

initialization
  New(GNidusInject);
  GNidusInject^ := TNidusInject.Create;
  GNidusInject^.CreateNidusdInject;

finalization
  if Assigned(GNidusInject) then
  begin
    GetNidus.Finalize;
    GNidusInject^.Free;
    Dispose(GNidusInject);
  end;

end.
