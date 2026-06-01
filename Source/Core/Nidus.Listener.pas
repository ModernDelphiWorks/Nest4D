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

unit Nidus.Listener;

interface

type
  TListener = reference to procedure(const AValue: String);

  TAppListener = class sealed
  strict private
    FListener: TListener;
  public
    constructor Create(const AListener: TListener);
    destructor Destroy; override;
    procedure Execute(const AValue: String);
  end;

implementation

{ TAppListener }

constructor TAppListener.Create(const AListener: TListener);
begin
  FListener := AListener;
end;

destructor TAppListener.Destroy;
begin
  FListener := nil;
  inherited;
end;

procedure TAppListener.Execute(const AValue: String);
begin
  if Assigned(FListener) then
    FListener(AValue);
end;

end.

