{
             Nest4D - Development Framework for Delphi


                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit nest4d.module.service;

interface

uses
  SysUtils,
  System.Evolution.ResultPair,
  nest4d.exception,
  nest4d.route.abstract,
  nest4d.module.abstract,
  nest4d.module.provider;

type
  TModuleService = class
  private
    FProvider: TModuleProvider;
  public
    destructor Destroy; override;
    procedure IncludeProvider(const AProvider: TModuleProvider);
    procedure Start(const AModule: TModuleAbstract;
      const AInitialRoutePath: String);
    procedure DisposeModule(const APath: String);
    procedure AddRoutes(const AModule: TModuleAbstract);
    procedure BindModule(const AModule: TModuleAbstract);
    procedure RemoveRoutes(const AModuleName: String);
    procedure ExtractInjector<T: class>(const ATag: String);
  end;

implementation

{ TModuleService }

procedure TModuleService.DisposeModule(
  const APath: String);
var
  LResult: TResultPair<Boolean, String>;
begin
  LResult := FProvider.DisposeModule(APath);
end;

procedure TModuleService.ExtractInjector<T>(const ATag: String);
begin
  FProvider.ExtractInjector<T>(ATag);
end;

procedure TModuleService.AddRoutes(const AModule: TModuleAbstract);
begin
  FProvider.AddRoutes(AModule);
end;

procedure TModuleService.BindModule(const AModule: TModuleAbstract);
begin
  FProvider.BindModule(AModule);
end;

destructor TModuleService.Destroy;
begin
  if Assigned(FProvider) then
    FProvider.Free;
  inherited;
end;

procedure TModuleService.IncludeProvider(const AProvider: TModuleProvider);
begin
  FProvider := AProvider;
end;

procedure TModuleService.RemoveRoutes(const AModuleName: String);
begin
  FProvider.RemoveRoutes(AModuleName);
end;

procedure TModuleService.Start(const AModule: TModuleAbstract;
  const AInitialRoutePath: String);
var
  LResult: TResultPair<Boolean, String>;
begin
  LResult := FProvider.Start(AModule, AInitialRoutePath);
end;

end.







