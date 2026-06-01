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

unit Nidus.Pooling.Helpers;

interface

uses
  SysUtils,
  Nidus.Pooling.Interfaces;

type
  TPoolHelpers = class sealed
  public
    class procedure WithPool<T: class>(const AProc: TProc<T>); overload; static;
    class procedure WithPool<T: class>(const AKey: string; const AProc: TProc<T>); overload; static;
  end;

implementation

class procedure TPoolHelpers.WithPool<T>(const AKey: string; const AProc: TProc<T>);
var
  LRegistry: IPoolRegistry;
  LPool: IPool;
  LObj: TObject;
  LValue: T;
begin
  if not Assigned(AProc) then
    Exit;

  LRegistry := GetGlobalPoolRegistry;
  if not Assigned(LRegistry) then
  begin
    AProc(nil);
    Exit;
  end;

  if not LRegistry.TryGetPool(AKey, LPool) then
  begin
    AProc(nil);
    Exit;
  end;

  LObj := LPool.Acquire;
  try
    if LObj is T then
      LValue := T(LObj)
    else
      LValue := nil;
    AProc(LValue);
  finally
    LPool.Release(LObj);
  end;
end;

class procedure TPoolHelpers.WithPool<T>(const AProc: TProc<T>);
begin
  WithPool<T>(T.ClassName, AProc);
end;

end.
