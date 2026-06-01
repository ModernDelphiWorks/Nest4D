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

unit Nidus.Route.Key;

interface

uses
  SysUtils;

type
  TRouteKey = record
  public
    Path: String;
    Schema: String;
    constructor Create(const APath: String; const ASchema: String);
    function CopyWith(const APath: String; const ASchema: String): TRouteKey;
    function GetHashCode: Int64;
    class operator Equal(const ALeft, ARight: TRouteKey): Boolean;
    class operator NotEqual(const ALeft, ARight: TRouteKey): Boolean;
  end;

implementation

{ TRouteKey }

constructor TRouteKey.Create(const APath: String; const ASchema: String);
begin
  Schema := ASchema;
  Path := APath;
end;

function TRouteKey.CopyWith(const APath: String; const ASchema: String): TRouteKey;
begin
  Result.Schema := ASchema;
  Result.Path := APath;
end;

class operator TRouteKey.Equal(const ALeft, ARight: TRouteKey): Boolean;
begin
  Result := (ALeft.Schema = ARight.Schema) and (ALeft.Path = ARight.Path);
end;

class operator TRouteKey.NotEqual(const ALeft, ARight: TRouteKey): Boolean;
begin
  Result := not (ALeft = ARight);
end;

function TRouteKey.GetHashCode: Int64;
begin
  Result := Schema.GetHashCode + Path.GetHashCode;
end;

end.








