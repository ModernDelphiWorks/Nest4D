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

unit Nidus.Message.Bus.Module;

interface

uses
  Nidus.Message.Bus,
  Nidus.Module;

type
  TMessageBusModule = class(TModule)
  public
    function ExportedBinds: TExportedBinds; override;
  end;

implementation

{ TEventEmitterModule }

function TMessageBusModule.ExportedBinds: TExportedBinds;
begin
  Result := [Bind<TMessageBus>.Singleton];
end;

end.





