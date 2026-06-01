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

unit Nidus.Bind.Abstract;

interface

uses
  SysUtils,
  Inject,
  Inject.Events;

type
  TBindAbstract<T: class, constructor> = class
  protected
    FOnCreate: TProc<T>;
    FOnDestroy: TProc<T>;
    FOnParams: TConstructorCallback;
    FAddInstance: TObject;
  public
    destructor Destroy; override;
    procedure IncludeInject(const ANidusInject: TInject); virtual; abstract;
  end;

implementation

destructor TBindAbstract<T>.Destroy;
begin
  FOnCreate := nil;
  FOnDestroy := nil;
  FOnParams := nil;
  FAddInstance := nil;
  inherited;
end;

end.





