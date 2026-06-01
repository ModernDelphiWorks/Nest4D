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

unit Nidus.Decorator.IsMax;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types,
  Nidus.Validation.IsMax;

type
  IsMaxAttribute = class(IsAttribute)
  private
    FValueMax: TValue;
  public
    constructor Create(const AValueMax: Extended; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsMaxAttribute }

constructor IsMaxAttribute.Create(const AValueMax: Extended; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMax';
  FValueMax := AValueMax;
end;

function IsMaxAttribute.Params: TArray<TValue>;
begin
  Result := [FValueMax];
end;

function IsMaxAttribute.Validation: TValidation;
begin
  Result := TIsMax;
end;

end.






