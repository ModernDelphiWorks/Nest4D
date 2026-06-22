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

unit Nidus.Route.Manager;

interface

uses
  SysUtils,
  ModernSyntax.Objects,
  Generics.Collections,
  RegularExpressions;

type
  TRouteManager = class
  private
    FEndPoints: TSmartPtr<TList<String>>;
  public
    constructor Create;
    function FindEndPoint(const ARoute: String): String;
    function RemoveSuffix(const ARoute: String): String;
    function EndPoints: TList<String>;
  end;

implementation

{ TRouteManager }

constructor TRouteManager.Create;
begin
  FEndPoints := TList<String>.Create;
end;

function TRouteManager.EndPoints: TList<String>;
begin
  Result := FEndPoints;
end;

function TRouteManager.FindEndPoint(const ARoute: String): String;
var
  LURI: String;
  LIndex: Integer;
begin
  Result := '';
  LURI := LowerCase(ARoute);
  LIndex := FEndpoints.AsRef.IndexOf(LURI);
  if LIndex > -1 then
    Result := FEndpoints.AsRef.Items[LIndex];
end;

function TRouteManager.RemoveSuffix(const ARoute: String): String;
const
  // DEC-051 — strip ALL trailing param segments (one or more), not just the last
  // one, so composite-key routes reduce to their literal prefix. The original
  // '(/{[^/]*})|(/:[^/]+)$' anchored only ONE trailing '/:param' to end-of-string,
  // so '/x/:a/:b' kept '/x/:a' (param still embedded) and never matched a request.
  // The repeated anchored group also folds in the (now brace-escaped) '/{param}'
  // form. '/x/:a' still reduces to '/x' (single-param behaviour unchanged).
  LPattern = '(/:[^/]+|/\{[^/]*\})+$';
begin
  Result := TRegEx.Replace(ARoute, LPattern, '');
end;

end.




