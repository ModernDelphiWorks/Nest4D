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
  @abstract(Nest4D Framework for Delphi)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://nest4d-en.docs-br.com)
}

unit nest4d.injector;

interface

uses
  Rtti,
  SysUtils,
  injector4d,
  injector4d.service,
  Generics.Collections;

type
  PAppInjector = ^TAppInjector;
  TAppInjector = class(TInjector4D)
  public
    procedure CreateNest4dInjector;
    procedure ExtractInjector<T: class>(const ATag: String = '');
  end;

  TCoreInjector = class(TAppInjector)
  private
    procedure _TrackeInjector; // Singleton
    procedure _ObjectFactoryInjector; // SingletonLazy
    procedure _BindServiceInjector; // Factory
    procedure _RouteServiceInjector; // Factory
    procedure _ModuleServiceInjector; // Factory
    procedure _ModuleProviderInjector; // Factory
    procedure _BindProviderInjector; // Factory
    procedure _RouteProviderInjector; // Factory
    procedure _RouteParseInjector; // Factory
    procedure _Nest4dBrInjector; // SingletonLazy
  public
    constructor Create; override;
  end;

var
  GAppInjector: PAppInjector = nil;

const C_NEST4D = 'Nest4D';

implementation

uses
  System.Evolution.Objects,
  nest4d.bind.provider,
  nest4d.bind.service,
  nest4d.module.provider,
  nest4d.module.service,
  nest4d.route.provider,
  nest4d.route.parse,
  nest4d.route.service,
  nest4d.route.manager,
  nest4d,
  nest4d.tracker,
  nest4d.register;

{ TCoreInjector }

constructor TCoreInjector.Create;
begin
  inherited;
  // Datasource
  _TrackeInjector;
  _ObjectFactoryInjector;
  // Infra
  _BindServiceInjector;
  _RouteServiceInjector;
  _ModuleServiceInjector;
  // Domain
  _ModuleProviderInjector;
  _BindProviderInjector;
  _RouteProviderInjector;
  _RouteParseInjector;
  _Nest4dBrInjector;
end;

procedure TCoreInjector._BindProviderInjector;
begin
  Self.Factory<TBindProvider>(nil, nil,
    function: TConstructorParams
    begin
      Result := [TValue.From<TTracker>(Self.Get<TTracker>)];
    end);
end;

procedure TCoreInjector._BindServiceInjector;
begin
  Self.Factory<TBindService>(
    procedure(Value: TBindService)
    begin
      Value.IncludeBindProvider(Self.Get<TBindProvider>);
    end);
end;

procedure TCoreInjector._Nest4dBrInjector;
begin
  Self.SingletonLazy<TNest4D>(
    procedure(Value: TNest4D)
    begin
      Value.IncludeModuleService(Self.Get<TModuleService>);
      Value.IncludeBindService(Self.Get<TBindService>);
      Value.IncludeRouteParser(Self.Get<TRouteParse>);
    end);
end;

procedure TCoreInjector._ModuleProviderInjector;
begin
  Self.Factory<TModuleProvider>(
    procedure(Value: TModuleProvider)
    begin
      Value.IncludeTracker(Self.Get<TTracker>);
    end);
end;

procedure TCoreInjector._ModuleServiceInjector;
begin
  Self.Factory<TModuleService>(
    procedure(Value: TModuleService)
    begin
      Value.IncludeProvider(Self.Get<TModuleProvider>);
    end);
end;

procedure TCoreInjector._RouteParseInjector;
begin
  Self.Factory<TRouteParse>(
    procedure(Value: TRouteParse)
    begin
      Value.IncludeRouteService(Self.Get<TRouteService>);
    end);
end;

procedure TCoreInjector._RouteProviderInjector;
begin
  Self.Factory<TRouteProvider>(
    procedure(Value: TRouteProvider)
    begin
      Value.IncludeTracker(Self.Get<TTracker>);
    end);
end;

procedure TCoreInjector._RouteServiceInjector;
begin
  Self.Factory<TRouteService>(
    procedure(Value: TRouteService)
    begin
      Value.IncludeProvider(Self.Get<TRouteProvider>);
    end);
end;

procedure TCoreInjector._TrackeInjector;
begin
  Self.SingletonLazy<TTracker>;
  Self.SingletonLazy<TRouteManager>;
end;

procedure TCoreInjector._ObjectFactoryInjector;
begin
  Self.Singleton<TEvolutionObject>;
end;

procedure TAppInjector.CreateNest4dInjector;
begin
  GAppInjector^.AddInjector(C_NEST4D, TCoreInjector.Create);
  GAppInjector^.AddInstance<TRegister>(TRegister.Create);
end;

procedure TAppInjector.ExtractInjector<T>(const ATag: String);
var
  LKey: String;
begin
  LKey := ATag;
  if LKey = '' then
    LKey := T.ClassName;
  Self.Remove<T>(LKey);
end;

initialization
  New(GAppInjector);
  GAppInjector^ := TAppInjector.Create;
  GAppInjector^.CreateNest4dInjector;

finalization
  if Assigned(GAppInjector) then
  begin
    GetNest4D.Finalize;
    GAppInjector^.Free;
    Dispose(GAppInjector);
  end;

end.







