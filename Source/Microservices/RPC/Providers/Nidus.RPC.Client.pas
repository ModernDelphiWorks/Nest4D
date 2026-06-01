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

unit Nidus.RPC.Client;

interface

uses
  Nidus.RPC.Interfaces;

type
  TRPCProviderClient = class(TInterfacedObject, IRPCProviderClient)
  public
    constructor Create(const AHost: String; const APort: integer = 8080); virtual;
    destructor Destroy; override;
    function ExecuteRPC(const ARequest: String): String; virtual;
  end;

implementation

{ TRPCProviderClient }

constructor TRPCProviderClient.Create(const AHost: String; const APort: integer);
begin

end;

destructor TRPCProviderClient.Destroy;
begin

  inherited;
end;

function TRPCProviderClient.ExecuteRPC(const ARequest: String): String;
begin

end;

end.






