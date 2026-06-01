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

unit Nidus.Route.Abstract;

interface

uses
 SysUtils,
  Generics.Collections,
  Nidus.Request;

type
  TRouteAbstract = class;

  IRouteMiddleware = interface
    ['{0962F508-FEA3-4109-9775-220C9F8B257E}']
    function Before(ARoute: TRouteAbstract): TRouteAbstract;
    function Call(const AReq: IRouteRequest): Boolean;
    procedure After(ARoute: TRouteAbstract);
  end;

  TRouteMiddleware = class(TInterfacedObject, IRouteMiddleware)
  public
    function Before(ARoute: TRouteAbstract): TRouteAbstract; virtual;
    function Call(const AReq: IRouteRequest): Boolean; virtual;
    procedure After(ARoute: TRouteAbstract); virtual;
  end;

  TRouteClass = class of TRouteAbstract;
  TRouteMiddlewareClass = class of TRouteMiddleware;
  TMiddlewares = array of TRouteMiddlewareClass;

  TRouteAbstract = class
  private
    FSchema: String;
    FPath: String;
    FParent: String;
    FModule: TClass;
    [weak]FModuleInstance: TObject;
    FRouteMiddlewares: TMiddlewares;
    procedure _SetSchema(const Value: String);
    procedure _SetPath(const Value: String);
    procedure _SetParent(const Value: String);
    procedure _SetModuleInstance(const Value: TObject);
  public
    constructor Create(const APath: String; const ASchema: String;
      const AModule: TClass; AMiddlewares: TMiddlewares); virtual;
    destructor Destroy; override;
    class function AddModule(const APath: String; const AModule: TClass;
      const AMiddlewares: TMiddlewares): TRouteAbstract; virtual; abstract;
    // Propertys
    property Schema: String read FSchema write _SetSchema;
    property Path: String read FPath write _SetPath;
    property Parent: String read FParent write _SetParent;
    property Module: TClass read FModule;
    property Middlewares: TMiddlewares read FRouteMiddlewares;
    property ModuleInstance: TObject read FModuleInstance write _SetModuleInstance;
  end;

implementation

{ TRoute }
constructor TRouteAbstract.Create(const APath: String; const ASchema: String;
  const AModule: TClass; AMiddlewares: TMiddlewares);
begin
  FPath := APath;
  FSchema := ASchema;
  FParent := ASchema;
  FModule := AModule;
  FRouteMiddlewares := AMiddlewares;
end;

destructor TRouteAbstract.Destroy;
begin
//  if FModuleInstance <> nil then
//    FModuleInstance.Free;
  inherited;
end;

procedure TRouteAbstract._SetModuleInstance(const Value: TObject);
begin
  FModuleInstance := Value;
end;

procedure TRouteAbstract._SetPath(const Value: String);
begin
  FPath := Value;
end;

procedure TRouteAbstract._SetParent(const Value: String);
begin
  FParent := Value;
end;

procedure TRouteAbstract._SetSchema(const Value: String);
begin
  FSchema := Value;
end;

{ TRouteMiddleware }

procedure TRouteMiddleware.After(ARoute: TRouteAbstract);
begin

end;

function TRouteMiddleware.Before(ARoute: TRouteAbstract): TRouteAbstract;
begin
  Result := ARoute;
end;

function TRouteMiddleware.Call(const AReq: IRouteRequest): Boolean;
begin
  Result := True;
end;

end.










