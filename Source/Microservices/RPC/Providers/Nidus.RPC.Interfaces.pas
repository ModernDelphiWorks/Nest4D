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

unit Nidus.RPC.Interfaces;

interface

uses
  Nidus.RPC.Resource;

type
  IRPCProviderServer = interface
    ['{B6ABE323-4FD8-49DF-9D1F-208FF424A872}']
    procedure Start;
    procedure Stop;
    procedure PublishRPC(const ARPCName: String; const ARPCClass: TRPCResourceClass);
    function ExecuteRPC(const AContext: String): String;
  end;

  IRPCProviderClient = interface
    ['{53B122DB-B9DB-434F-A0DA-4A8EE44EA842}']
    function ExecuteRPC(const AContext: String): String;
  end;

  IRPCRouteHandle = interface
    ['{7C0BFB60-92F3-430B-A119-479A07F58EC4}']
    procedure PublishRPC(const ARPCName: String; const ARPCClass: TRPCResourceClass);
    function ExecuteRPC(const AContext: String): String;
  end;

implementation

end.







