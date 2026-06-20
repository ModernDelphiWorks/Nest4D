---
title: Object Pooling
displayed_sidebar: nidusSidebar
sidebar_position: 5
---

# Object Pooling

Nidus provides a thread-safe generic object pool (`TObjectPool<T>`) and a `TComponent` variant. Pooling eliminates repeated allocation of expensive objects under high concurrency.

## Registering a pool

### Generic objects

```delphi
GetNidus
  .UsePools<TMyHeavyResource>(128)   // max pool size = 128
  .Start(TAppModule.Create);
```

This calls `TPoolRegistry.RegisterDefaultObjectPool<TMyHeavyResource>(128)` internally.

### TComponent subclasses

```delphi
GetNidus
  .UsePools<TMyDbConnection>(32, AOwner,
    procedure(C: TMyDbConnection)
    begin
      C.Reset;   // called before returning to pool
    end)
  .Start(TAppModule.Create);
```

Or the equivalent alias:

```delphi
GetNidus.UseComponentPool<TMyDbConnection>(32, AOwner, ResetProc);
```

### Custom registry

Provide your own `IPoolRegistry` implementation:

```delphi
GetNidus.UsePools(TMyPoolRegistry.Create);
```

## Acquiring and releasing objects

### Via `WithPool`

The safest pattern — automatically acquires and releases around the callback:

```delphi
GetNidus.WithPool<TMyHeavyResource>(
  procedure(R: TMyHeavyResource)
  begin
    R.DoWork;
  end);
```

With a named key (when multiple pools exist for the same type):

```delphi
GetNidus.WithPool<TMyHeavyResource>('v2',
  procedure(R: TMyHeavyResource)
  begin
    R.DoWork;
  end);
```

Source: `TNidus.WithPool<T>` delegates to `TPoolHelpers.WithPool<T>` (`Nidus.Pooling.Helpers`).

### Via `IPoolRegistry` directly

```delphi
var LPool := GetNidus.Pools;
var LObj  := LPool.Acquire<TMyHeavyResource>;
try
  LObj.DoWork;
finally
  LPool.Release(LObj);
end;
```

## Pool internals

`TObjectPool<T>` wraps a `TQueue<T>` protected by a `TCriticalSection`. Behaviour:

- `Acquire`: dequeues an existing instance if available; otherwise calls `T.Create`.
- `Release`: enqueues the instance if `Count < MaxSize`; otherwise frees it.
- `MaxSize` defaults to 256 for object pools, 32 for component pools.

Source: `Nidus.ObjectPool` — `TObjectPool<T>`, `TObjectPoolAdapter<T>`.

## Accessing the pool registry

```delphi
var LRegistry := GetNidus.Pools;   // returns IPoolRegistry
```

`SetGlobalPoolRegistry` / `GetGlobalPoolRegistry` manage the global singleton.
Source: `Nidus.Pooling.Interfaces`.
