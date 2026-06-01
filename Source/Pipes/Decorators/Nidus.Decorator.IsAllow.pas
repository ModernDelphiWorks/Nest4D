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

unit Nidus.Decorator.IsAllow;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  IsAllowAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ IsArrayAttribute }

constructor IsAllowAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsAllow';
end;

function IsAllowAttribute.Validation: TValidation;
begin
//  Result := TIsArray;
end;

end.





