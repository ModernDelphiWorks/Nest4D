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

unit Nidus.Decorator.IsLength;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types,
  Nidus.Validation.IsLength;

type
  IsLengthAttribute = class(IsAttribute)
  private
    FValueMin: TValue;
    FValueMax: TValue;
  public
    constructor Create(const AValueMin: Extended; const AValueMax: Extended;
      const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsLengthAttribute }

constructor IsLengthAttribute.Create(const AValueMin: Extended;
  const AValueMax: Extended; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsLength';
  FValueMin := AValueMin;
  FValueMax := AValueMax;
end;

function IsLengthAttribute.Params: TArray<TValue>;
begin
  Result := [FValueMin, FValueMax];
end;

function IsLengthAttribute.Validation: TValidation;
begin
  Result := TIsLength;
end;

end.





