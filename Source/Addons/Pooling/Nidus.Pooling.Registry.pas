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

unit Nidus.Pooling.Registry;

interface

uses
  SysUtils,
  Classes,
  SyncObjs,
  Generics.Collections,
  Nidus.ObjectPool,
  Nidus.Pooling.FactoryPool,
  Nidus.Pooling.Interfaces;

type
  TPoolRegistry = class(TInterfacedObject, IPoolRegistry)
  private
    FLock: TCriticalSection;
    FPools: TDictionary<string, IPool>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterPool(const AKey: string; const APool: IPool);
    function TryGetPool(const AKey: string; out APool: IPool): Boolean;
    function RegisterObjectPool<T: class, constructor>(const AKey: string;
      const AMaxSize: Integer = 256): TPoolRegistry;
    function RegisterFactoryPool(const AKey: string; const AFactory: TFunc<TObject>;
      const AReset: TProc<TObject> = nil; const AMaxSize: Integer = 256): TPoolRegistry;

    class function EnsureGlobalRegistry: IPoolRegistry; static;
    class procedure RegisterDefaultObjectPool<T: class, constructor>(const AMaxSize: Integer = 256); static;
    class procedure RegisterDefaultComponentPool<T: TComponent>(const AMaxSize: Integer = 32;
      const AOwner: TComponent = nil; const AReset: TProc<T> = nil); static;
  end;

implementation

constructor TPoolRegistry.Create;
begin
  FLock := TCriticalSection.Create;
  FPools := TDictionary<string, IPool>.Create;
end;

destructor TPoolRegistry.Destroy;
begin
  if Assigned(FPools) then
    FPools.Free;
  if Assigned(FLock) then
    FLock.Free;
  inherited;
end;

procedure TPoolRegistry.RegisterPool(const AKey: string; const APool: IPool);
var
  LKey: string;
begin
  LKey := Trim(LowerCase(AKey));
  if (LKey = '') or (not Assigned(APool)) then
    Exit;
  FLock.Enter;
  try
    FPools.AddOrSetValue(LKey, APool);
  finally
    FLock.Leave;
  end;
end;

function TPoolRegistry.TryGetPool(const AKey: string; out APool: IPool): Boolean;
var
  LKey: string;
begin
  LKey := Trim(LowerCase(AKey));
  APool := nil;
  if LKey = '' then
    Exit(False);
  FLock.Enter;
  try
    Result := FPools.TryGetValue(LKey, APool);
  finally
    FLock.Leave;
  end;
end;

function TPoolRegistry.RegisterObjectPool<T>(const AKey: string; const AMaxSize: Integer): TPoolRegistry;
begin
  RegisterPool(AKey, TObjectPoolAdapter<T>.CreateDefault(AMaxSize));
  Result := Self;
end;

function TPoolRegistry.RegisterFactoryPool(const AKey: string; const AFactory: TFunc<TObject>;
  const AReset: TProc<TObject>; const AMaxSize: Integer): TPoolRegistry;
begin
  RegisterPool(AKey, TFactoryPool.Create(AFactory, AReset, AMaxSize));
  Result := Self;
end;

class function TPoolRegistry.EnsureGlobalRegistry: IPoolRegistry;
begin
  Result := GetGlobalPoolRegistry;
  if Assigned(Result) then
    Exit;
  Result := TPoolRegistry.Create;
  SetGlobalPoolRegistry(Result);
end;

class procedure TPoolRegistry.RegisterDefaultObjectPool<T>(const AMaxSize: Integer);
var
  LRegistry: IPoolRegistry;
begin
  LRegistry := EnsureGlobalRegistry;
  LRegistry.RegisterPool(T.ClassName, TObjectPoolAdapter<T>.CreateDefault(AMaxSize));
end;

class procedure TPoolRegistry.RegisterDefaultComponentPool<T>(const AMaxSize: Integer;
  const AOwner: TComponent; const AReset: TProc<T>);
var
  LRegistry: IPoolRegistry;
begin
  LRegistry := EnsureGlobalRegistry;
  LRegistry.RegisterPool(
    T.ClassName,
    TFactoryPool.Create(
      function: TObject
      begin
        Result := T.Create(AOwner);
      end,
      procedure (O: TObject)
      begin
        if Assigned(AReset) and (O is T) then
          AReset(T(O));
      end,
      AMaxSize
    )
  );
end;

end.
