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

unit Nidus.Decorator.IsString;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types,
  Nidus.Validation.IsString;

type
  IsStringAttribute = class(IsAttribute)
  private
    FEach: Boolean;
    FGroups: TArray<String>;
  public
    constructor Create(const AEach: Boolean = False;
      const AMessage: String = ''; const AGroups: TArray<String> = nil); reintroduce;
    function Validation: TValidation; override;
  end;

implementation

{ IsStringAttribute }

constructor IsStringAttribute.Create(const AEach: Boolean;
  const AMessage: String; const AGroups: TArray<String>);
begin
  inherited Create(AMessage);
  FTagName := 'IsString';
  FEach := AEach;
  FGroups := AGroups;
end;

function IsStringAttribute.Validation: TValidation;
begin
  Result := TIsString;
end;

end.







