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

unit Nidus.ObjectPool;

interface

uses
  SysUtils,
  SyncObjs,
  Generics.Collections,
  Nidus.Pooling.Interfaces;

type
  TObjectPoolOptions = record
  public
    MaxSize: Integer;
    constructor Create(const AMaxSize: Integer);
  end;

  TObjectPool<T: class, constructor> = class
  private
    FLock: TCriticalSection;
    FItems: TQueue<T>;
    FMaxSize: Integer;
  public
    constructor Create(const AOptions: TObjectPoolOptions);
    destructor Destroy; override;
    function Acquire: T;
    procedure Release(const AValue: T);
    function Count: Integer;
  end;

  TObjectPoolAdapter<T: class, constructor> = class(TInterfacedObject, IPool)
  private
    FPool: TObjectPool<T>;
  public
    constructor Create(const APool: TObjectPool<T>);
    destructor Destroy; override;
    class function CreateDefault(const AMaxSize: Integer = 256): IPool; static;
    function Acquire: TObject;
    procedure Release(const AValue: TObject);
    function Count: Integer;
  end;

implementation

constructor TObjectPoolOptions.Create(const AMaxSize: Integer);
begin
  MaxSize := AMaxSize;
end;

constructor TObjectPool<T>.Create(const AOptions: TObjectPoolOptions);
begin
  FLock := TCriticalSection.Create;
  FItems := TQueue<T>.Create;
  FMaxSize := AOptions.MaxSize;
  if FMaxSize <= 0 then
    FMaxSize := 256;
end;

destructor TObjectPool<T>.Destroy;
var
  LItem: T;
begin
  if Assigned(FLock) then
    FLock.Enter;
  try
    if Assigned(FItems) then
    begin
      while FItems.Count > 0 do
      begin
        LItem := FItems.Dequeue;
        LItem.Free;
      end;
      FItems.Free;
    end;
  finally
    if Assigned(FLock) then
    begin
      FLock.Leave;
      FLock.Free;
    end;
  end;
  inherited;
end;

function TObjectPool<T>.Acquire: T;
begin
  FLock.Enter;
  try
    if FItems.Count > 0 then
      Exit(FItems.Dequeue);
  finally
    FLock.Leave;
  end;
  Result := T.Create;
end;

procedure TObjectPool<T>.Release(const AValue: T);
begin
  if not Assigned(AValue) then
    Exit;
  FLock.Enter;
  try
    if FItems.Count >= FMaxSize then
    begin
      AValue.Free;
      Exit;
    end;
    FItems.Enqueue(AValue);
  finally
    FLock.Leave;
  end;
end;

function TObjectPool<T>.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FItems.Count;
  finally
    FLock.Leave;
  end;
end;

constructor TObjectPoolAdapter<T>.Create(const APool: TObjectPool<T>);
begin
  inherited Create;
  FPool := APool;
end;

destructor TObjectPoolAdapter<T>.Destroy;
begin
  if Assigned(FPool) then
    FPool.Free;
  inherited;
end;

class function TObjectPoolAdapter<T>.CreateDefault(const AMaxSize: Integer): IPool;
begin
  Result := TObjectPoolAdapter<T>.Create(
    TObjectPool<T>.Create(TObjectPoolOptions.Create(AMaxSize))
  );
end;

function TObjectPoolAdapter<T>.Acquire: TObject;
begin
  Result := FPool.Acquire;
end;

procedure TObjectPoolAdapter<T>.Release(const AValue: TObject);
begin
  if AValue is T then
    FPool.Release(T(AValue))
  else if Assigned(AValue) then
    AValue.Free;
end;

function TObjectPoolAdapter<T>.Count: Integer;
begin
  Result := FPool.Count;
end;

end.

