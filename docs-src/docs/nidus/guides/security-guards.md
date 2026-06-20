---
title: Security Guards
displayed_sidebar: nidusSidebar
sidebar_position: 4
---

# Security Guards

Guards intercept every incoming request before it reaches a route handler. If a guard returns `False`, Nidus raises `EUnauthorizedException` (HTTP 401) and stops processing immediately.

## Registering a guard

```delphi
GetNidus
  .UseGuard(
    function: Boolean
    begin
      // Return True to allow, False to deny
      Result := ValidateJWT(GetNidus.Request.Authorization);
    end)
  .Start(TAppModule.Create);
```

`UseGuard` accepts a `TGuardCallback` — a reference function that returns `Boolean`. The callback is stored in `TNidus.FGuardCallback` and evaluated on each call to `LoadRouteModule`.

## How it fits in the request pipeline

```
LoadRouteModule(path, request)
  │
  ├── [guard set?] → FGuardCallback()
  │       └── False → raise EUnauthorizedException (HTTP 401)
  │
  ├── [pipe set?]  → Pipe.Validate(routeHandler, request)
  │       └── messages? → return TReturnPair.Failure(EBadRequestException)
  │
  └── RouteParse.SelectRoute(path, request) → success
```

Source: `TNidus.LoadRouteModule` in `Nidus.pas`.

## Accessing request data inside a guard

Inside the guard callback, use `GetNidus.Request` to inspect the current `IRouteRequest`:

```delphi
.UseGuard(
  function: Boolean
  var LReq: IRouteRequest;
  begin
    LReq := GetNidus.Request;
    Result := (LReq <> nil) and CheckToken(LReq.Authorization);
  end)
```

`IRouteRequest` (defined in `Nidus.Request`) exposes the following methods:

| Method | Return type | Description |
|---|---|---|
| `Header` | `TRequestData` | Request headers |
| `Params` | `TRequestData` | Route parameters |
| `Querys` | `TRequestData` | Query-string parameters |
| `Body` | `string` | Raw request body |
| `Host` | `string` | Host name |
| `ContentType` | `string` | `Content-Type` header value |
| `Method` | `string` | HTTP method (`GET`, `POST`, …) |
| `URL` | `string` | Full request URL |
| `Port` | `integer` | Server port |
| `Authorization` | `string` | `Authorization` header value |
| `AsObject` | `TObject` | Deserialized body object (set by `TParseJsonPipe`) |

## Exception types

| Exception | HTTP status | Raised by |
|---|---|---|
| `EUnauthorizedException` | 401 | Guard returns `False` |
| `EBadRequestException` | 400 | Validation pipe failure |
| `ENidusException` (base) | status from `.Status` | Any Nidus exception |

Source: `Nidus.Exception`.
