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

unit Nidus.Pooling.FactoryPool;

interface

uses
  SysUtils,
  SyncObjs,
  Generics.Collections,
  Nidus.Pooling.Interfaces;

type
  TFactoryPool = class(TInterfacedObject, IPool)
  private
    FLock: TCriticalSection;
    FItems: TQueue<TObject>;
    FFactory: TFunc<TObject>;
    FReset: TProc<TObject>;
    FMaxSize: Integer;
  public
    constructor Create(const AFactory: TFunc<TObject>; const AReset: TProc<TObject> = nil;
      const AMaxSize: Integer = 256);
    destructor Destroy; override;
    function Acquire: TObject;
    procedure Release(const AValue: TObject);
    function Count: Integer;
  end;

implementation

constructor TFactoryPool.Create(const AFactory: TFunc<TObject>; const AReset: TProc<TObject>;
  const AMaxSize: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FItems := TQueue<TObject>.Create;
  FFactory := AFactory;
  FReset := AReset;
  FMaxSize := AMaxSize;
  if FMaxSize <= 0 then
    FMaxSize := 256;
end;

destructor TFactoryPool.Destroy;
var
  LItem: TObject;
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

function TFactoryPool.Acquire: TObject;
begin
  Result := nil;
  FLock.Enter;
  try
    if FItems.Count > 0 then
      Exit(FItems.Dequeue);
  finally
    FLock.Leave;
  end;
  if Assigned(FFactory) then
    Result := FFactory();
end;

procedure TFactoryPool.Release(const AValue: TObject);
begin
  if not Assigned(AValue) then
    Exit;

  if Assigned(FReset) then
    FReset(AValue);

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

function TFactoryPool.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FItems.Count;
  finally
    FLock.Leave;
  end;
end;

end.

