---
title: Quickstart
displayed_sidebar: nidusSidebar
sidebar_position: 2
---

# Quickstart

This guide walks you through a minimal Nidus application in five steps.

## 1. Add the `uses` clause

```delphi
uses
  Nidus,
  Nidus.Module,      // TModule base class
  Nidus.Inject;      // initialises GNidusInject (include in .dpr)
```

Including `Nidus.Inject` in the project's `.dpr` (or any unit that loads before `GetNidus` is called) runs the `initialization` section, which creates the internal DI container.

## 2. Define a module

```delphi
type
  [Module]
  TUserModule = class(TModule)
  public
    procedure Configure(const ABuilder: IModuleBuilder); override;
  end;

procedure TUserModule.Configure(const ABuilder: IModuleBuilder);
begin
  ABuilder
    .AddProvider(TBind<TUserService>.Singleton)
    .Export(TUserService);
end;
```

## 3. Define a DTO with validation decorators

```delphi
uses
  Nidus.Decorator.IsEmail,
  Nidus.Decorator.IsNotEmpty,
  Nidus.Decorator.IsStrongPassword;

type
  TCreateUserDto = class
  private
    FName: string;
    FEmail: string;
    FPassword: string;
  public
    [IsNotEmpty('Name is required!')]
    property Name: string read FName write FName;

    [IsEmail('Invalid email address!')]
    property Email: string read FEmail write FEmail;

    [IsStrongPassword('Password is too weak!')]
    property Password: string read FPassword write FPassword;
  end;
```

## 4. Bootstrap the application

```delphi
var
  LApp: TNidus;
begin
  LApp := GetNidus;
  try
    LApp
      .UseListener(TMyLogger.Log)           // structured log callback
      .UsePipes(TValidationPipe.Create)      // global RTTI validation
      .UseCache(TMemoryCache.Create)         // module-level cache
      .UsePools<TMyHeavyResource>(128)       // object pool, max 128 items
      .Start(TAppModule.Create);             // boot the root module

    // Resolve a service by type
    var LUserService := LApp.Get<TUserService>;
    LUserService.Register('Isaque Pinheiro', 'isaquesp@gmail.com');
  finally
    LApp.Finalize;
  end;
end;
```

## 5. Integrate with Horse (HTTP)

```delphi
uses
  Horse,
  Nidus.Driver.Horse;

begin
  THorse
    .Use(Nidus_Horse(TAppModule.Create))   // boots Nidus + returns middleware
    .Get('/users', ...)
    .Listen(9000);
end;
```

`Nidus_Horse(TAppModule)` calls `GetNidus.Start(AAppModule)` internally and returns the `Middleware` callback that Horse executes on every request.

## Next steps

- [Modular Architecture guide](../guides/modular-architecture)
- [Validation Pipes guide](../guides/validation-pipes)
- [Horse Integration guide](../guides/horse-integration)
