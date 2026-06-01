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

unit Nidus.Route.Param;

interface

uses
  SysUtils,
  Math,
  Rtti,
  Nidus.Request;

type
  TRouteParam = record
  private
    FPath: String;
    FSchema: String;
    FRequest: IRouteRequest;
  public
    constructor Create(const APath: String;
      const AReq: IRouteRequest = nil; const ASchema: String = '');
    procedure ResolveURL;
    property Path: String read FPath;
    property Schema: String read FSchema;
    property Request: IRouteRequest read Frequest;
  end;

implementation

constructor TRouteParam.Create(const APath: String;
  const AReq: IRouteRequest; const ASchema: String);
begin
  FPath := APath;
  FSchema := ASchema;
  FRequest := AReq;
end;

procedure TRouteParam.ResolveURL;
begin
  FPath := FPath + '/';
end;

end.








