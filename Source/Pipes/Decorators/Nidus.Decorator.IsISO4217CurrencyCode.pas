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

unit Nidus.Decorator.IsISO4217CurrencyCode;

interface

uses
  SysUtils,
  Nidus.Decorator.IsBase,
  Nidus.Validation.Types;

type
  isiso4217currencycodeAttribute = class(IsAttribute)
  public
    constructor Create(const AMessage: String = ''); override;
    function Validation: TValidation; override;
  end;

implementation

{ isiso4217currencycodeAttribute }

constructor isiso4217currencycodeAttribute.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'isiso4217currencycode';
end;

function isiso4217currencycodeAttribute.Validation: TValidation;
begin
  // TODO: Implementar validacao isiso4217currencycode quando disponivel
  Result := nil;
end;

end.

