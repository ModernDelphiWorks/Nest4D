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

unit Nidus.RPC.Exception;

interface

uses
  SysUtils;

type
  ERPCJSONException = class(Exception)
  public
    constructor Create(const Msg: String);
  end;

  ERPCJSONParamsException = class(Exception)
  public
    constructor Create(const Msg: String);
  end;

implementation

constructor ERPCJSONException.Create(const Msg: String);
begin
  inherited Create(Msg);
end;

{ EJSONRPCParamsException }

constructor ERPCJSONParamsException.Create(const Msg: String);
begin
  inherited Create(Msg);
end;

end.



