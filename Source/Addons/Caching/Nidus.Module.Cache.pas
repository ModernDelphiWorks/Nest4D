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

unit Nidus.Module.Cache;

interface

uses
  SysUtils,
  SyncObjs,
  Generics.Collections,
  Nidus.Module.Cache.Interfaces;

type
  TModuleCacheManager = class(TInterfacedObject, IModuleCache)
  private
    FLock: TCriticalSection;
    FInstances: TObjectDictionary<string, TObject>;
    FEnabled: TDictionary<string, Byte>;
    FCacheAll: Boolean;
    function KeyOf(const AModuleClass: TClass): string;
    function IsAllMarker(const AModuleClass: TClass): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function ResolveInstance(const AModuleClass: TClass; const AFactory: TFunc<TObject>): TObject;
    procedure Invalidate(const AModuleClass: TClass);
    procedure Clear;
    function SetPolicy(const AModuleClasses: array of TClass): IModuleCache; overload;
    function EnableAll: IModuleCache;
    function DisableAll: IModuleCache;
    function IsEnabledFor(const AModuleClass: TClass): Boolean;
  end;

implementation

constructor TModuleCacheManager.Create;
begin
  FLock := TCriticalSection.Create;
  FInstances := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  FEnabled := TDictionary<string, Byte>.Create;
  FCacheAll := False;
end;

destructor TModuleCacheManager.Destroy;
begin
  if Assigned(FInstances) then
    FInstances.Free;
  if Assigned(FEnabled) then
    FEnabled.Free;
  if Assigned(FLock) then
    FLock.Free;
  inherited;
end;

function TModuleCacheManager.KeyOf(const AModuleClass: TClass): string;
begin
  if AModuleClass = nil then
    Exit('');
  Result := AModuleClass.QualifiedClassName;
  if Result = '' then
    Result := AModuleClass.ClassName;
end;

function TModuleCacheManager.IsAllMarker(const AModuleClass: TClass): Boolean;
begin
  Result := (AModuleClass = TNidusCacheAll) or AModuleClass.InheritsFrom(TNidusCacheAll);
end;

function TModuleCacheManager.SetPolicy(const AModuleClasses: array of TClass): IModuleCache;
var
  LModuleClass: TClass;
  LKey: string;
begin
  FLock.Enter;
  try
    for LModuleClass in AModuleClasses do
    begin
      if not Assigned(LModuleClass) then
        Continue;
      if IsAllMarker(LModuleClass) then
      begin
        FCacheAll := True;
        Continue;
      end;
      LKey := KeyOf(LModuleClass);
      if LKey <> '' then
        FEnabled.AddOrSetValue(LKey, 1);
    end;
  finally
    FLock.Leave;
  end;
  Result := Self;
end;

function TModuleCacheManager.EnableAll: IModuleCache;
begin
  FLock.Enter;
  try
    FCacheAll := True;
  finally
    FLock.Leave;
  end;
  Result := Self;
end;

function TModuleCacheManager.DisableAll: IModuleCache;
begin
  FLock.Enter;
  try
    FCacheAll := False;
    FEnabled.Clear;
    Clear;
  finally
    FLock.Leave;
  end;
  Result := Self;
end;

function TModuleCacheManager.IsEnabledFor(const AModuleClass: TClass): Boolean;
var
  LKey: string;
  LValue: Byte;
begin
  if not Assigned(AModuleClass) then
    Exit(False);
  LKey := KeyOf(AModuleClass);
  if LKey = '' then
    Exit(False);
  FLock.Enter;
  try
    if FCacheAll then
      Exit(True);
    Result := FEnabled.TryGetValue(LKey, LValue);
  finally
    FLock.Leave;
  end;
end;

function TModuleCacheManager.ResolveInstance(const AModuleClass: TClass; const AFactory: TFunc<TObject>): TObject;
var
  LKey: string;
begin
  Result := nil;
  if not Assigned(AModuleClass) then
    Exit;

  if not IsEnabledFor(AModuleClass) then
  begin
    if Assigned(AFactory) then
      Result := AFactory();
    Exit;
  end;

  LKey := KeyOf(AModuleClass);

  FLock.Enter;
  try
    if FInstances.TryGetValue(LKey, Result) then
      Exit;
    if not Assigned(AFactory) then
      Exit;
    Result := AFactory();
    if Assigned(Result) then
      FInstances.AddOrSetValue(LKey, Result);
  finally
    FLock.Leave;
  end;
end;

procedure TModuleCacheManager.Invalidate(const AModuleClass: TClass);
var
  LKey: string;
begin
  LKey := KeyOf(AModuleClass);
  if LKey = '' then
    Exit;
  FLock.Enter;
  try
    if FInstances.ContainsKey(LKey) then
      FInstances.Remove(LKey);
  finally
    FLock.Leave;
  end;
end;

procedure TModuleCacheManager.Clear;
begin
  FLock.Enter;
  try
    FInstances.Clear;
  finally
    FLock.Leave;
  end;
end;

end.

