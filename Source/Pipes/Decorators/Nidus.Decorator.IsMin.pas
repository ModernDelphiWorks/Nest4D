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

unit Nidus.Decorator.IsMin;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types,
  Nidus.Validation.IsMin;

type
  IsMinAttribute = class(IsAttribute)
  private
    FValueMin: TValue;
  public
    constructor Create(const AValueMin: Extended; const AMessage: String = ''); reintroduce;
    function Validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsMinAttribute }

constructor IsMinAttribute.Create(const AValueMin: Extended; const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'IsMin';
  FValueMin := AValueMin;
end;

function IsMinAttribute.Params: TArray<TValue>;
begin
  Result := [FValueMin];
end;

function IsMinAttribute.Validation: TValidation;
begin
  Result := TIsMin;
end;

end.





