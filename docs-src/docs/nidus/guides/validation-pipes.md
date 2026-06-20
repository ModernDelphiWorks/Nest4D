---
title: Validation Pipes
displayed_sidebar: nidusSidebar
sidebar_position: 3
---

# Validation Pipes

Nidus provides a rich validation system built on Delphi RTTI attributes. Annotate DTO class properties with **decorator attributes** and register a global `TValidationPipe` — Nidus will validate incoming request data automatically before route handlers run.

## Enabling global validation

```delphi
GetNidus
  .UsePipes(TValidationPipe.Create)
  .Start(TAppModule.Create);
```

Source: `TNidus.UsePipes` stores the `IValidationPipe` in `TRegister`. On every request, `LoadRouteModule` calls `FRegister.Pipe.Validate(LRouteHandle, FRequest)`.

## Annotating a DTO

```delphi
uses
  Nidus.Decorator.IsEmail,
  Nidus.Decorator.IsNotEmpty,
  Nidus.Decorator.IsStrongPassword,
  Nidus.Decorator.IsUUID,
  Nidus.Decorator.IsInteger;

type
  TCreateUserDto = class
  private
    FName: string;
    FEmail: string;
    FPassword: string;
    FAge: Integer;
  public
    [IsNotEmpty('Name is required!')]
    property Name: string read FName write FName;

    [IsEmail('Invalid email address!')]
    property Email: string read FEmail write FEmail;

    [IsStrongPassword('Password is too weak!')]
    property Password: string read FPassword write FPassword;

    [IsInteger('Age must be an integer')]
    property Age: Integer read FAge write FAge;
  end;
```

## Full decorator catalogue

Nidus ships with over 60 built-in decorators under `Source/Pipes/Decorators/`. Key families:

### String validators
| Decorator | Unit |
|---|---|
| `[IsNotEmpty(msg)]` | `Nidus.Decorator.IsNotEmpty` |
| `[IsEmpty(msg)]` | `Nidus.Decorator.IsEmpty` |
| `[IsAlpha(msg)]` | `Nidus.Decorator.IsAlpha` |
| `[IsAlphaNumeric(msg)]` | `Nidus.Decorator.IsAlphaNumeric` |
| `[IsAscii(msg)]` | `Nidus.Decorator.IsAscii` |
| `[Contains(sub, msg)]` | `Nidus.Decorator.Contains` |
| `[IsLength(min,max,msg)]` <!-- TODO: confirm param names --> | `Nidus.Decorator.IsLength` |

### Format validators
| Decorator | Unit |
|---|---|
| `[IsEmail(msg)]` | `Nidus.Decorator.IsEmail` |
| `[IsUUID(msg)]` | `Nidus.Decorator.IsUUID` |
| `[IsIP(msg)]` | `Nidus.Decorator.IsIP` |
| `[IsURL(msg)]` | `Nidus.Decorator.IsURL` |
| `[IsISO8601(msg)]` | `Nidus.Decorator.IsISO8601` |
| `[IsDate(msg)]` | `Nidus.Decorator.IsDate` |
| `[IsDateString(msg)]` | `Nidus.Decorator.IsDateString` |
| `[IsHexColor(msg)]` | `Nidus.Decorator.IsHexColor` |
| `[IsHexadecimal(msg)]` | `Nidus.Decorator.IsHexadecimal` |
| `[IsCreditCard(msg)]` | `Nidus.Decorator.IsCreditCard` |
| `[IsIban(msg)]` | `Nidus.Decorator.IsIban` |
| `[IsBic(msg)]` | `Nidus.Decorator.IsBic` |
| `[IsEAN(msg)]` | `Nidus.Decorator.IsEAN` |
| `[IsISBN(msg)]` | `Nidus.Decorator.IsIsbn` |

### Type validators
| Decorator | Unit |
|---|---|
| `[IsInteger(msg)]` | `Nidus.Decorator.IsInteger` |
| `[IsNumber(msg)]` | `Nidus.Decorator.IsNumber` |
| `[IsBoolean(msg)]` | `Nidus.Decorator.IsBoolean` |
| `[IsBooleanString(msg)]` | `Nidus.Decorator.IsBooleanString` |
| `[IsEnum(TEnum, msg)]` | `Nidus.Decorator.IsEnum` |
| `[IsInstance(TClass, msg)]` | `Nidus.Decorator.IsInstance` |

### Crypto / encoding
| Decorator | Unit |
|---|---|
| `[IsBase64(msg)]` | `Nidus.Decorator.IsBase64` |
| `[IsBase32(msg)]` | `Nidus.Decorator.IsBase32` |
| `[IsBase58(msg)]` | `Nidus.Decorator.IsBase58` |
| `[IsHash(alg, msg)]` | `Nidus.Decorator.IsHash` |
| `[IsBTCAddress(msg)]` | `Nidus.Decorator.IsBTCAddress` |
| `[IsEthereumAddress(msg)]` | `Nidus.Decorator.IsEethereumAddress` |

### Array validators
| Decorator | Unit |
|---|---|
| `[IsArray(msg)]` | `Nidus.Decorator.IsArray` |
| `[IsArrayNotEmpty(msg)]` | `Nidus.Decorator.IsArrayNotEmpty` |
| `[IsArrayUnique(msg)]` | `Nidus.Decorator.IsArrayUnique` |
| `[IsArrayMinSize(n, msg)]` | `Nidus.Decorator.IsArrayMinSize` |
| `[IsArrayMaxSize(n, msg)]` | `Nidus.Decorator.IsArrayMaxSize` |
| `[ArrayContains(val, msg)]` | `Nidus.Decorator.ArrayContains` |
| `[ArrayNotContains(val, msg)]` | `Nidus.Decorator.ArrayNotContains` |

## Validation error response

When any property fails validation, `TValidationPipe.BuildMessages` aggregates the error messages and `LoadRouteModule` returns a `TReturnPair` failure wrapping `EBadRequestException`. The Horse driver translates this to an HTTP 400 response with a JSON body.

## Custom validators

Implement `IValidatorConstraint` (from `Nidus.Validation.Interfaces`) to write your own constraint, then create a custom attribute that references it. <!-- TODO: confirm — no example found in source -->

## Transform pipes

Two built-in transform pipes convert raw request strings to typed values:

| Class | Unit | Purpose |
|---|---|---|
| `TParseJsonPipe` | `Nidus.Parse.Json.Pipe` | Deserialize a JSON body string |
| `TParseIntegerPipe` | `Nidus.Parse.Integer.Pipe` | Parse a string route param to `Integer` |

Source barrel: `Nidus.Validation.Include`.
