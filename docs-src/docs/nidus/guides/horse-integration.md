---
title: Horse Integration
displayed_sidebar: nidusSidebar
sidebar_position: 9
---

# Horse Integration

Nidus ships a first-class middleware adapter for the [Horse](https://github.com/HashLoad/horse) HTTP framework. The adapter unit is `Nidus.Driver.Horse` and the bundled `Horse.ResponseCache` adds HTTP-level response caching.

## Quick setup

```delphi
uses
  Horse,
  Nidus.Driver.Horse;

begin
  THorse
    .Use(Nidus_Horse(TAppModule.Create))
    .Listen(9000);
end;
```

`Nidus_Horse(TAppModule)` does two things:

1. Calls `GetNidus.Start(TAppModule)` to bootstrap the Nidus module tree.
2. Returns the `Middleware` callback that Horse calls on every request.

## The middleware callback

`procedure Middleware(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)` is the core Horse middleware. Its flow:

1. **Skip Swagger / favicon routes** — paths containing `swagger` or `favicon.ico` pass through immediately.
2. **Build `IRouteRequest`** — `_ResolverRouteRequest(Req)` wraps all Horse request properties into `TRouteRequest`.
3. **Load route module** — `GetNidus.LoadRouteModule(PathInfo, LRequest)` runs guards + pipes + route dispatch.
4. **Error mapping** — `ENidusException` subclasses are mapped to their `.Status` code; other exceptions → HTTP 500.
5. **Dispose** — `GetNidus.DisposeRouteModule(PathInfo)` is called in the `finally` block to free route-scoped module instances.

## `IRouteRequest` fields

`TRouteRequest` is constructed from the Horse request:

| Field | Source |
|---|---|
| `Headers` | `Req.Headers.Content` |
| `Params` | `Req.Params.Content` |
| `Query` | `Req.Query.Content` |
| `Body` | `Req.Body` |
| `Host` | `Req.RawWebRequest.Host` |
| `ContentType` | `Req.RawWebRequest.ContentType` |
| `Method` | `Req.RawWebRequest.Method` |
| `PathInfo` | `Req.RawWebRequest.PathInfo` |
| `ServerPort` | `Req.RawWebRequest.ServerPort` |
| `Authorization` | `Req.RawWebRequest.Authorization` |

## HTTP methods supported

The middleware only processes `mtGet`, `mtPost`, `mtPut`, `mtPatch`, `mtDelete`. Other methods (e.g. `mtOptions`) pass straight through to `Next`.

## Charset / content-type override

If you need a different charset:

```delphi
THorse
  .Use(Nidus_Horse('UTF-8'))   // returns middleware only, no Start
  .Use(Nidus_Horse(TAppModule.Create));
```

## `Horse.ResponseCache`

`Horse.ResponseCache` (`Source/Horse/Horse.ResponseCache.pas`) is a Nidus-owned Horse middleware for HTTP response caching. <!-- TODO: confirm full API — unit exists but was not read -->

## Error JSON format

Horse middleware wraps Nidus errors in JSON:

```json
// ENidusException
{ "statusCode": 401, "message": "Unauthorized" }

// EHorseException
{ "statusCode": 404, "message": "Not found" }

// Generic Exception
{
  "statusCode": "400",
  "scope": "MyUnit",
  "message": "Something went wrong"
}
```
