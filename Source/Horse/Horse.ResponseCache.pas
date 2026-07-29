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

unit Horse.ResponseCache;

interface

uses
  DateUtils,
  SysUtils,
  SyncObjs,
  Generics.Collections,
  Web.HTTPApp,
  Horse,
  Horse.Commons;  // must stay after Web.HTTPApp: Horse 4.x declares its own TMethodType

type
  THorseResponseCacheOptions = class
  private
    FTtlSeconds: Integer;
    FMaxEntries: Integer;
    FVaryAuthorization: Boolean;
    FCacheAll: Boolean;
    FCachePrefixes: TArray<string>;
    FSkipRoutes: TArray<string>;
    function NormalizePath(const APath: string): string;
    function IsSkippedPath(const APath: string): Boolean;
    function IsCacheEnabledForPath(const APath: string): Boolean;
  public
    constructor Create;
    function TtlSeconds(const ASeconds: Integer): THorseResponseCacheOptions;
    function MaxEntries(const AMaxEntries: Integer): THorseResponseCacheOptions;
    function VaryAuthorization(const AEnabled: Boolean): THorseResponseCacheOptions;
    function CacheAll(const AEnabled: Boolean = True): THorseResponseCacheOptions;
    function CacheRoutes(const ARoutes: array of string): THorseResponseCacheOptions;
    function SkipRoutes(const ARoutes: array of string): THorseResponseCacheOptions;
  end;

  THorseResponseCache = class
  private
    type
      TEntry = record
      public
        StatusCode: Integer;
        ContentType: string;
        Body: string;
        ExpiresAt: TDateTime;
      end;
      TStore = class
      private
        FLock: TCriticalSection;
        FItems: TDictionary<string, TEntry>;
        function NowUtc: TDateTime;
        procedure CleanupExpired_NoLock;
      public
        constructor Create;
        destructor Destroy; override;
        function TryGet(const AKey: string; out AEntry: TEntry): Boolean;
        procedure SetValue(const AKey: string; const AEntry: TEntry; const ATtlSeconds: Integer;
          const AMaxEntries: Integer);
      end;
  end;

function ResponseCache(const AOptions: THorseResponseCacheOptions = nil): THorseCallback; overload;
function ResponseCache(const ATtlSeconds: Integer; const AMaxEntries: Integer = 5000;
  const AVaryAuthorization: Boolean = True): THorseCallback; overload;
function ResponseCache(const ACacheRoutes: array of string; const ATtlSeconds: Integer = 30;
  const AMaxEntries: Integer = 5000; const AVaryAuthorization: Boolean = True): THorseCallback; overload;

implementation

constructor THorseResponseCacheOptions.Create;
begin
  FTtlSeconds := 30;
  FMaxEntries := 5000;
  FVaryAuthorization := True;
  FCacheAll := True;
  FCachePrefixes := [];
  FSkipRoutes := ['/swagger', '/favicon.ico'];
end;

function THorseResponseCacheOptions.TtlSeconds(const ASeconds: Integer): THorseResponseCacheOptions;
begin
  if ASeconds > 0 then
    FTtlSeconds := ASeconds;
  Result := Self;
end;

function THorseResponseCacheOptions.MaxEntries(const AMaxEntries: Integer): THorseResponseCacheOptions;
begin
  if AMaxEntries > 0 then
    FMaxEntries := AMaxEntries;
  Result := Self;
end;

function THorseResponseCacheOptions.VaryAuthorization(const AEnabled: Boolean): THorseResponseCacheOptions;
begin
  FVaryAuthorization := AEnabled;
  Result := Self;
end;

function THorseResponseCacheOptions.CacheAll(const AEnabled: Boolean): THorseResponseCacheOptions;
begin
  FCacheAll := AEnabled;
  Result := Self;
end;

function THorseResponseCacheOptions.CacheRoutes(const ARoutes: array of string): THorseResponseCacheOptions;
var
  LRoute: string;
  LList: TList<string>;
begin
  LList := TList<string>.Create;
  try
    for LRoute in ARoutes do
      LList.Add(NormalizePath(LRoute));
    FCachePrefixes := LList.ToArray;
  finally
    LList.Free;
  end;
  FCacheAll := Length(FCachePrefixes) = 0;
  Result := Self;
end;

function THorseResponseCacheOptions.SkipRoutes(const ARoutes: array of string): THorseResponseCacheOptions;
var
  LRoute: string;
  LList: TList<string>;
begin
  LList := TList<string>.Create;
  try
    for LRoute in ARoutes do
      LList.Add(NormalizePath(LRoute));
    FSkipRoutes := LList.ToArray;
  finally
    LList.Free;
  end;
  Result := Self;
end;

function THorseResponseCacheOptions.NormalizePath(const APath: string): string;
begin
  Result := Trim(LowerCase(APath));
  if Result = '' then
    Exit('/');
  if not Result.StartsWith('/') then
    Result := '/' + Result;
end;

function THorseResponseCacheOptions.IsSkippedPath(const APath: string): Boolean;
var
  LNeedle: string;
  LPath: string;
begin
  LPath := NormalizePath(APath);
  for LNeedle in FSkipRoutes do
  begin
    if (LNeedle <> '') and (Pos(LNeedle, LPath) > 0) then
      Exit(True);
  end;
  Result := False;
end;

function THorseResponseCacheOptions.IsCacheEnabledForPath(const APath: string): Boolean;
var
  LPath: string;
  LPrefix: string;
begin
  if FCacheAll then
    Exit(True);
  LPath := NormalizePath(APath);
  for LPrefix in FCachePrefixes do
  begin
    if (LPrefix <> '') and LPath.StartsWith(LPrefix) then
      Exit(True);
  end;
  Result := False;
end;

constructor THorseResponseCache.TStore.Create;
begin
  FLock := TCriticalSection.Create;
  FItems := TDictionary<string, THorseResponseCache.TEntry>.Create;
end;

destructor THorseResponseCache.TStore.Destroy;
begin
  if Assigned(FItems) then
    FItems.Free;
  if Assigned(FLock) then
    FLock.Free;
  inherited;
end;

function THorseResponseCache.TStore.NowUtc: TDateTime;
begin
  Result := TTimeZone.Local.ToUniversalTime(Now);
end;

procedure THorseResponseCache.TStore.CleanupExpired_NoLock;
var
  LKey: string;
  LKeys: TArray<string>;
  LEntry: THorseResponseCache.TEntry;
begin
  LKeys := FItems.Keys.ToArray;
  for LKey in LKeys do
  begin
    if FItems.TryGetValue(LKey, LEntry) then
    begin
      if (LEntry.ExpiresAt > 0) and (LEntry.ExpiresAt <= NowUtc) then
        FItems.Remove(LKey);
    end;
  end;
end;

function THorseResponseCache.TStore.TryGet(const AKey: string; out AEntry: THorseResponseCache.TEntry): Boolean;
begin
  Result := False;
  AEntry := Default(THorseResponseCache.TEntry);
  if AKey = '' then
    Exit;

  FLock.Enter;
  try
    CleanupExpired_NoLock;
    Result := FItems.TryGetValue(AKey, AEntry);
    if Result and (AEntry.ExpiresAt > 0) and (AEntry.ExpiresAt <= NowUtc) then
    begin
      FItems.Remove(AKey);
      Result := False;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure THorseResponseCache.TStore.SetValue(const AKey: string; const AEntry: THorseResponseCache.TEntry;
  const ATtlSeconds: Integer; const AMaxEntries: Integer);
var
  LEntry: THorseResponseCache.TEntry;
begin
  if AKey = '' then
    Exit;
  LEntry := AEntry;
  if (ATtlSeconds > 0) and (LEntry.ExpiresAt = 0) then
    LEntry.ExpiresAt := NowUtc + (ATtlSeconds / SecsPerDay);

  FLock.Enter;
  try
    CleanupExpired_NoLock;
    if (AMaxEntries > 0) and (FItems.Count >= AMaxEntries) then
      CleanupExpired_NoLock;
    if (AMaxEntries > 0) and (FItems.Count >= AMaxEntries) then
      Exit;
    FItems.AddOrSetValue(AKey, LEntry);
  finally
    FLock.Leave;
  end;
end;

function ResponseCache(const AOptions: THorseResponseCacheOptions): THorseCallback;
var
  LOptions: THorseResponseCacheOptions;
  LStore: THorseResponseCache.TStore;
begin
  LOptions := AOptions;
  if not Assigned(LOptions) then
    LOptions := THorseResponseCacheOptions.Create;
  LStore := THorseResponseCache.TStore.Create;

  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LKey: string;
      LAuth: string;
      LEntry: THorseResponseCache.TEntry;
    begin
      if Req.MethodType <> mtGet then
      begin
        Next;
        Exit;
      end;

      if LOptions.IsSkippedPath(Req.RawWebRequest.PathInfo) then
      begin
        Next;
        Exit;
      end;

      if not LOptions.IsCacheEnabledForPath(Req.RawWebRequest.PathInfo) then
      begin
        Next;
        Exit;
      end;

      if LOptions.FVaryAuthorization then
        LAuth := Req.RawWebRequest.Authorization
      else
        LAuth := '';

      LKey := LowerCase(Req.RawWebRequest.Method) + ':' + Req.RawWebRequest.PathInfo + '?' + Req.RawWebRequest.Query + '|auth=' + LAuth;
      if LStore.TryGet(LKey, LEntry) then
      begin
        Res.Send(LEntry.Body).Status(LEntry.StatusCode).ContentType(LEntry.ContentType);
        raise EHorseCallbackInterrupted.Create;
      end;

      Next;

      if (Res.RawWebResponse.StatusCode >= 200) and (Res.RawWebResponse.StatusCode <= 299) and (Res.RawWebResponse.Content <> '') then
      begin
        LEntry.StatusCode := Res.RawWebResponse.StatusCode;
        LEntry.ContentType := Res.RawWebResponse.ContentType;
        LEntry.Body := Res.RawWebResponse.Content;
        LEntry.ExpiresAt := 0;
        LStore.SetValue(LKey, LEntry, LOptions.FTtlSeconds, LOptions.FMaxEntries);
      end;
    end
end;

function ResponseCache(const ATtlSeconds: Integer; const AMaxEntries: Integer;
  const AVaryAuthorization: Boolean): THorseCallback;
var
  LOptions: THorseResponseCacheOptions;
begin
  LOptions := THorseResponseCacheOptions.Create
    .TtlSeconds(ATtlSeconds)
    .MaxEntries(AMaxEntries)
    .VaryAuthorization(AVaryAuthorization);
  Result := ResponseCache(LOptions);
end;

function ResponseCache(const ACacheRoutes: array of string; const ATtlSeconds: Integer;
  const AMaxEntries: Integer; const AVaryAuthorization: Boolean): THorseCallback;
var
  LOptions: THorseResponseCacheOptions;
begin
  LOptions := THorseResponseCacheOptions.Create
    .TtlSeconds(ATtlSeconds)
    .MaxEntries(AMaxEntries)
    .VaryAuthorization(AVaryAuthorization)
    .CacheRoutes(ACacheRoutes);
  Result := ResponseCache(LOptions);
end;

end.
