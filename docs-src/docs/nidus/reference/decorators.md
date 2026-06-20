---
title: Decorator Reference
displayed_sidebar: nidusSidebar
sidebar_position: 2
---

# Decorator Reference

All built-in validation decorators live under `Source/Pipes/Decorators/`. Each is a Delphi RTTI attribute that annotates a DTO property. The `TValidationPipe` scans the RTTI of the DTO class at runtime and invokes the corresponding constraint.

## Usage pattern

```delphi
uses
  Nidus.Decorator.IsEmail,
  Nidus.Decorator.IsNotEmpty;

type
  TMyDto = class
  public
    [IsNotEmpty('Field is required')]
    property Name: string read FName write FName;

    [IsEmail('Must be a valid email')]
    property Email: string read FEmail write FEmail;
  end;
```

The `msg` parameter is the error message returned when validation fails.

## Common / string validators

| Attribute | Unit | Description |
|---|---|---|
| `[IsNotEmpty(msg)]` | `Nidus.Decorator.IsNotEmpty` | Value must not be empty or whitespace |
| `[IsEmpty(msg)]` | `Nidus.Decorator.IsEmpty` | Value must be empty |
| `[IsDefined(msg)]` | `Nidus.Decorator.IsDefined` | Value must not be `nil` / unassigned |
| `[IsEquals(other, msg)]` | `Nidus.Decorator.IsEquals` | Value must equal `other` |
| `[IsIn(vals, msg)]` | `Nidus.Decorator.IsIn` | Value must be in the allowed set |
| `[IsAlpha(msg)]` | `Nidus.Decorator.IsAlpha` | Letters only |
| `[IsAlphaNumeric(msg)]` | `Nidus.Decorator.IsAlphaNumeric` | Letters and digits only |
| `[IsAscii(msg)]` | `Nidus.Decorator.IsAscii` | ASCII characters only |
| `[Contains(sub, msg)]` | `Nidus.Decorator.Contains` | Must contain substring |
| `[IsFullWidth(msg)]` | `Nidus.Decorator.IsFullWidth` | Full-width characters |
| `[IsHalfWidth(msg)]` | `Nidus.Decorator.IsHalfWidth` | Half-width characters |

## Numeric validators

| Attribute | Unit | Description |
|---|---|---|
| `[IsInteger(msg)]` | `Nidus.Decorator.IsInteger` | Must be an integer |
| `[IsNumber(msg)]` | `Nidus.Decorator.IsNumber` | Must be a number |
| `[IsDivisibleBy(n, msg)]` | `Nidus.Decorator.IsDivisibleBy` | Must be divisible by `n` |
| `[IsByteLength(min, max, msg)]` | `Nidus.Decorator.IsByteLength` | Byte length in range |

## Type validators

| Attribute | Unit | Description |
|---|---|---|
| `[IsBoolean(msg)]` | `Nidus.Decorator.IsBoolean` | Must be a boolean |
| `[IsBooleanString(msg)]` | `Nidus.Decorator.IsBooleanString` | Must be `"true"` / `"false"` |
| `[IsEnum(TEnum, msg)]` | `Nidus.Decorator.IsEnum` | Must be a valid enum value |
| `[IsInstance(TClass, msg)]` | `Nidus.Decorator.IsInstance` | Must be an instance of `TClass` |
| `[IsAllow(vals, msg)]` | `Nidus.Decorator.IsAllow` | Must be in the allowed set |

## Format validators

| Attribute | Unit | Description |
|---|---|---|
| `[IsEmail(msg)]` | `Nidus.Decorator.IsEmail` | RFC 5322 email |
| `[IsUUID(msg)]` <!-- TODO: confirm --> | — | UUID format |
| `[IsIP(msg)]` | `Nidus.Decorator.IsIP` | IPv4 or IPv6 |
| `[IsURL(msg)]` <!-- TODO: confirm --> | — | URL format |
| `[IsISO8601(msg)]` | `Nidus.Decorator.IsISO8601` | ISO 8601 date-time |
| `[IsDate(msg)]` | `Nidus.Decorator.IsDate` | Date value |
| `[IsDateString(msg)]` | `Nidus.Decorator.IsDateString` | Date string |
| `[IsHexColor(msg)]` | `Nidus.Decorator.IsHexColor` | Hex color (`#RRGGBB`) |
| `[IsHexadecimal(msg)]` | `Nidus.Decorator.IsHexadecimal` | Hexadecimal string |
| `[IsHsl(msg)]` | `Nidus.Decorator.IsHsl` | HSL color |
| `[IsCurrency(msg)]` | `Nidus.Decorator.IsCurrency` | Currency format |
| `[IsDataURI(msg)]` | `Nidus.Decorator.IsDataURI` | Data URI |
| `[IsGqdn(msg)]` | `Nidus.Decorator.IsGqdn` | Fully qualified domain name |

## Financial / identity

| Attribute | Unit | Description |
|---|---|---|
| `[IsCreditCard(msg)]` | `Nidus.Decorator.IsCreditCard` | Credit card number (Luhn) |
| `[IsIban(msg)]` | `Nidus.Decorator.IsIban` | IBAN |
| `[IsBic(msg)]` | `Nidus.Decorator.IsBic` | BIC/SWIFT code |
| `[IsISO4217CurrencyCode(msg)]` | `Nidus.Decorator.IsISO4217CurrencyCode` | ISO 4217 currency |
| `[IsISO31661Alpha2(msg)]` | `Nidus.Decorator.IsISO31661Alpha2` | ISO 3166-1 alpha-2 country |
| `[IsISO31661Alpha3(msg)]` | `Nidus.Decorator.IsISO31661Alpha3` | ISO 3166-1 alpha-3 country |
| `[IsIdentityCard(msg)]` | `Nidus.Decorator.IsIdentityCard` | Identity card number |
| `[IsIsin(msg)]` | `Nidus.Decorator.IsIsin` | ISIN securities code |
| `[IsIsbn(msg)]` | `Nidus.Decorator.IsIsbn` | ISBN |
| `[IsEAN(msg)]` | `Nidus.Decorator.IsEAN` | EAN barcode |
| `[IsIson(msg)]` | `Nidus.Decorator.IsIson` | ISON |

## Encoding / crypto

| Attribute | Unit | Description |
|---|---|---|
| `[IsBase64(msg)]` | `Nidus.Decorator.IsBase64` | Base64 |
| `[IsBase32(msg)]` | `Nidus.Decorator.IsBase32` | Base32 |
| `[IsBase58(msg)]` | `Nidus.Decorator.IsBase58` | Base58 |
| `[IsBase(n, msg)]` | `Nidus.Decorator.IsBase` | Base-N encoding |
| `[IsHash(alg, msg)]` | `Nidus.Decorator.IsHash` | Hash string |
| `[IsBTCAddress(msg)]` | `Nidus.Decorator.IsBTCAddress` | Bitcoin address |
| `[IsEthereumAddress(msg)]` | `Nidus.Decorator.IsEethereumAddress` | Ethereum address |
| `[IsFirebasePushId(msg)]` | `Nidus.Decorator.IsFirebasePushId` | Firebase Push ID |

## Array validators

| Attribute | Unit | Description |
|---|---|---|
| `[IsArray(msg)]` | `Nidus.Decorator.IsArray` | Must be an array |
| `[IsArrayNotEmpty(msg)]` | `Nidus.Decorator.IsArrayNotEmpty` | Array must not be empty |
| `[IsArrayUnique(msg)]` | `Nidus.Decorator.IsArrayUnique` | All elements must be unique |
| `[IsArrayMinSize(n, msg)]` | `Nidus.Decorator.IsArrayMinSize` | Minimum element count |
| `[IsArrayMaxSize(n, msg)]` | `Nidus.Decorator.IsArrayMaxSize` | Maximum element count |
| `[ArrayContains(val, msg)]` | `Nidus.Decorator.ArrayContains` | Array must contain `val` |
| `[ArrayNotContains(val, msg)]` | `Nidus.Decorator.ArrayNotContains` | Array must not contain `val` |

## Password strength

| Attribute | Unit | Description |
|---|---|---|
| `[IsStrongPassword(msg)]` | `Nidus.Decorator.IsStrongPassword` <!-- TODO: confirm unit name --> | Must meet strong-password criteria |

## Strong-password criteria

<!-- TODO: confirm exact criteria — source unit not read -->
Typically: minimum 8 characters, at least one uppercase, one lowercase, one digit, one special character.

## Custom constraints

Implement `IValidatorConstraint` from `Nidus.Validation.Interfaces` to create your own constraint, then wrap it in a custom RTTI attribute. <!-- TODO: confirm example -->
