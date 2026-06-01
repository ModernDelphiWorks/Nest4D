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

unit Nidus.Bind.Service;

interface

uses
  SysUtils,
  TypInfo,
  ModernSyntax.ResultPair,
  Nidus.Bind.Provider;

type
  TBindService = class
  private
    FProvider: TBindProvider;
  public
    destructor Destroy; override;
    procedure IncludeBindProvider(const AProvider: TBindProvider);
    function GetBind<T: class, constructor>(const ATag: string): TResultPair<T, Exception>;
    function GetBindInterface<I: IInterface>(const ATag: string): TResultPair<I, Exception>;
  end;

implementation

uses
  Nidus.Exception;

{ TBindService }

destructor TBindService.Destroy;
begin
  if Assigned(FProvider) then
    FProvider.Free;
  inherited;
end;

//function TBindService.GetBindInterface<I>(const ATag: string): TResultPair<I, Exception>;
//begin
//  try
//    Result := FProvider.GetBindInterface<I>(ATag);
//    if Result.ValueSuccess = nil then
//      Result.Failure(EBindNotFoundException.Create(''));
//  except
//    on E: Exception do
//      Result.Failure(EBindException.Create(E.Message));
//  end;
//end;

function TBindService.GetBindInterface<I>(const ATag: String): TResultPair<I, Exception>;
var
  LTypeName: String;
  LMsg: String;
begin
  try
    Result := FProvider.GetBindInterface<I>(ATag);
    if Result.ValueSuccess = nil then
    begin
      LTypeName := GetTypeName(TypeInfo(I));
      LMsg := Format(
        '{"statusCode":"404","message":"Interface not found","error":"Not Found","bindType":"%s","tag":"%s"}',
        [LTypeName, ATag]
      );
      Result.Failure(EBindNotFoundException.Create(LMsg));
    end;
  except
    on E: Exception do
      Result.Failure(EBindException.Create(E.Message));
  end;
end;

//function TBindService.GetBind<T>(const ATag: string): TResultPair<T, Exception>;
//begin
//  try
//    Result := FProvider.GetBind<T>(ATag);
//    if Result.ValueSuccess = nil then
//      Result.Failure(EBindNotFoundException.Create(''));
//  except
//    on E: Exception do
//      Result.Failure(EBindException.Create(E.Message));
//  end;
//end;

function TBindService.GetBind<T>(const ATag: string): TResultPair<T, Exception>;
var
  LTypeName: string;
  LMsg: string;
begin
  try
    Result := FProvider.GetBind<T>(ATag);
    if Result.ValueSuccess = nil then
    begin
      LTypeName := T.ClassName;
      LMsg := Format(
        '{"statusCode":"404","message":"Class not found","error":"Not Found","bindType":"%s","tag":"%s"}',
        [LTypeName, ATag]
      );
      Result.Failure(EBindNotFoundException.Create(LMsg));
    end;
  except
    on E: Exception do
      Result.Failure(EBindException.Create(E.Message));
  end;
end;

procedure TBindService.IncludeBindProvider(const AProvider: TBindProvider);
begin
  FProvider := AProvider;
end;

end.

