---
title: Troubleshooting
displayed_sidebar: nidusSidebar
sidebar_position: 1
---

# Troubleshooting

## `EAppInjector` raised at startup

**Symptom:** `TNidus.Create` raises `EAppInjector`.

**Cause:** `GNidusInject` is `nil` — the `Nidus.Inject` unit was not loaded before `GetNidus` was called.

**Fix:** Add `Nidus.Inject` to the `uses` clause of your project's `.dpr` file (or any unit that is loaded before the first call to `GetNidus`). The `initialization` section of `Nidus.Inject` creates `GNidusInject`.

```delphi
// myproject.dpr
uses
  Nidus.Inject,   // ← must appear early
  Nidus,
  ...
```

---

## `EModuleStartedException` raised

**Symptom:** `TNidus.Start` raises `EModuleStartedException`.

**Cause:** `Start` was called more than once on the same `TNidus` instance.

**Fix:** Call `Start` exactly once per application lifetime. If you need to restart, call `Finalize` first and then obtain a fresh `TNidus` via `GetNidus` (after re-creating the inject container).

---

## HTTP 401 on every request

**Symptom:** All requests return HTTP 401 Unauthorized immediately after registering a guard.

**Cause:** The `TGuardCallback` always returns `False`, or it accesses `GetNidus.Request` before the request is assigned.

**Fix:**

```delphi
.UseGuard(
  function: Boolean
  var LReq: IRouteRequest;
  begin
    LReq := GetNidus.Request;
    if LReq = nil then
      Exit(False);
    Result := ValidateToken(LReq.Authorization);
  end)
```

---

## HTTP 400 with validation errors

**Symptom:** Route returns HTTP 400 even though the payload looks valid.

**Cause:** The validation pipe found one or more failing decorator constraints on the DTO.

**Fix:**

1. Check the JSON body key names match the DTO property names (case-sensitive RTTI lookup). <!-- TODO: confirm case sensitivity -->
2. Ensure the decorator attribute parameters are correct (e.g. `[IsEmail('msg')]` not `[IsEmail]`). <!-- TODO: confirm whether message param is required -->
3. Temporarily disable `UsePipes` to isolate whether the error comes from validation.

---

## `ERPCProviderNotSetException` raised

**Symptom:** `PublishRPC` raises `ERPCProviderNotSetException`.

**Cause:** `UseRPC(...)` was not called before `PublishRPC`.

**Fix:**

```delphi
GetNidus
  .UseRPC(TRPCServerIndy.Create('0.0.0.0', 9900))
  .PublishRPC('MyMethod', TMyRPCResource)
  .Start(TAppModule.Create);
```

---

## Module instances not freed between requests

**Symptom:** Memory grows across requests; module constructors only called once.

**Cause:** Module caching is enabled for the module (either via `SetPolicy` or `EnableAll`).

**Fix:** Remove the module from the cache policy or call `GetNidus.Cache.Invalidate(TYourModule)` to evict it.

---

## Linux64: `OutputDebugString` linker error

**Symptom:** Linker error mentioning `OutputDebugString` when building for Linux64.

**Cause:** An older version of `Nidus.Exception` called `OutputDebugString` unconditionally.

**Fix:** Update to the current source. The `DEBUG` trace in `Nidus.Exception` is now wrapped in `{$IFDEF MSWINDOWS}` with a `stderr` fallback on POSIX. No code change required in your application.

---

## `Nidus_Horse` route not dispatching

**Symptom:** Horse routes are not reached — all requests return empty or error.

**Cause:** The Nidus middleware was registered after the route handlers, so it never calls `Next`.

**Fix:** Ensure `Nidus_Horse` middleware is added **before** route handlers:

```delphi
THorse
  .Use(Nidus_Horse(TAppModule.Create))  // ← middleware first
  .Get('/users', HandleGetUsers)
  .Listen(9000);
```

---

## Pool `Acquire` always returns a new instance

**Symptom:** `TObjectPool<T>.Count` stays at 0; every `Acquire` creates a new object.

**Cause:** `Release` is never called, so objects are never returned to the pool.

**Fix:** Use `TNidus.WithPool<T>` which guarantees release in all paths, or wrap manual acquire/release in `try/finally`:

```delphi
GetNidus.WithPool<TMyResource>(
  procedure(R: TMyResource)
  begin
    R.DoWork;
  end);
```
