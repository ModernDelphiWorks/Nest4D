{
  Nest4D - Development Framework for Delphi

  Copyright (c) 2023, Isaque Pinheiro
  All rights reserved.

  GNU Lesser General Public License
  Vers?o 3, 29 de junho de 2007

  Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
  A todos ? permitido copiar e distribuir c?pias deste documento de
  licen?a, mas mud?-lo n?o ? permitido.

  Esta vers?o da GNU Lesser General Public License incorpora
  os termos e condi??es da vers?o 3 da GNU General Public License
  Licen?a, complementado pelas permiss?es adicionais listadas no
  arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework for Delphi)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://nest4d-en.docs-br.com)
}

unit Nest.Decorator.Contains;

interface

uses
  SysUtils,
  decorator.isbase,
  Nest.validation.types,
  validation.contains;

type
  ContainsAttribute = class(IsAttribute)
  private
    FValue: String;
  public
    constructor Create(const AValue: String; const AMessage: String = '');
      reintroduce;
    function validation: TValidation; override;
    function Params: TArray<TValue>; override;
  end;

implementation

{ IsArrayAttribute }

constructor ContainsAttribute.Create(const AValue: String;
  const AMessage: String);
begin
  inherited Create(AMessage);
  FTagName := 'Contains';
  FValue := AValue;
end;

function ContainsAttribute.Params: TArray<TValue>;
begin
  Result := [FValue];
end;

function ContainsAttribute.validation: TValidation;
begin
  Result := TContains;
end;

end.


