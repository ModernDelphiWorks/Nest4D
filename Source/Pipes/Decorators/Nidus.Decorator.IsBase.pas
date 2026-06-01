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

unit Nidus.Decorator.IsBase;

interface

uses
  SysUtils,
  Nidus.Validation.Types;

type
  IsAttribute = class(TCustomAttribute)
  protected
    FTagName: String;
    FMessage: String;
  public
    constructor Create(const AMessage: String = ''); virtual;
    function TagName: String;
    function Message: String;
    function Validation: TValidation; virtual; abstract;
    function Params: TArray<TValue>; virtual;
  end;

implementation

{ IsAttribute }

constructor IsAttribute.Create(const AMessage: String);
begin
  FTagName := '';
  FMessage := AMessage;
end;

function IsAttribute.Message: String;
begin
  Result := FMessage;
end;

function IsAttribute.Params: TArray<TValue>;
begin
  Result := [];
end;

function IsAttribute.TagName: String;
begin
  Result := FTagName;
end;

end.




