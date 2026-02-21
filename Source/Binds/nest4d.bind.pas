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
  @abstract(Nest4D Framework)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit Nest4D.Bind;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Nest4D.Bind.Abstract,
  Nest4D.Injector;

type
  TBind<T: class, constructor> = class(TBindAbstract<T>)
  public
    constructor Create(const AOnCreate: TProc<T>;
      const AOnDestroy: TProc<T>;
      const AOnConstructorParams: TConstructorCallback); overload;
    class function Singleton(const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil): TBind<TObject>;
    class function SingletonLazy(const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil): TBind<TObject>;
    class function Factory(const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil): TBind<TObject>;
    class function SingletonInterface<I: IInterface>(const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil): TBind<TObject>;
    class function AddInstance(const AInstance: TObject): TBind<TObject>;
  end;

  TSingletonBind<T: class, constructor> = class(TBind<T>)
  public
    procedure IncludeInjector(const AAppInjector: TAppInjector); override;
  end;

  TSingletonLazyBind<T: class, constructor> = class(TBind<T>)
  public
    procedure IncludeInjector(const AAppInjector: TAppInjector); override;
  end;

  TFactoryBind<T: class, constructor> = class(TBind<T>)
  public
    procedure IncludeInjector(const AAppInjector: TAppInjector); override;
  end;

  TSingletonInterfaceBind<I: IInterface; T: class, constructor> = class(TBind<T>)
  public
    procedure IncludeInjector(const AAppInjector: TAppInjector); override;
  end;

  TAddInstanceBind<T: class, constructor> = class(TBind<T>)
  public
    constructor Create(const AInstance: TObject); overload;
    procedure IncludeInjector(const AAppInjector: TAppInjector); override;
  end;

implementation

{ TBind<T> }

constructor TBind<T>.Create(const AOnCreate: TProc<T>; const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback);
begin
  FOnCreate := AOnCreate;
  FOnDestroy := AOnDestroy;
  FOnParams := AOnConstructorParams;
end;

class function TBind<T>.Factory(const AOnCreate: TProc<T>; const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback): TBind<TObject>;
begin
  Result := TBind<TObject>(TFactoryBind<T>.Create(AOnCreate,
                                                  AOnDestroy,
                                                  AOnConstructorParams));
end;

class function TBind<T>.Singleton(const AOnCreate: TProc<T>; const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback): TBind<TObject>;
begin
  Result := TBind<TObject>(TSingletonBind<T>.Create(AOnCreate,
                                                    AOnDestroy,
                                                    AOnConstructorParams));
end;

class function TBind<T>.SingletonInterface<I>(const AOnCreate: TProc<T>; const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback): TBind<TObject>;
begin
  Result := TBind<TObject>(TSingletonInterfaceBind<I, T>.Create(AOnCreate,
                                                                AOnDestroy,
                                                                AOnConstructorParams));
end;

class function TBind<T>.SingletonLazy(const AOnCreate: TProc<T>; const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback): TBind<TObject>;
begin
  Result := TBind<TObject>(TSingletonLazyBind<T>.Create(AOnCreate,
                                                        AOnDestroy,
                                                        AOnConstructorParams));
end;

class function TBind<T>.AddInstance(const AInstance: TObject): TBind<TObject>;
begin
  Result := TBind<TObject>(TAddInstanceBind<T>.Create(AInstance));
end;

{ TSingletonBind }

procedure TSingletonBind<T>.IncludeInjector(const AAppInjector: TAppInjector);
begin
  AAppInjector.Singleton<T>(FOnCreate, FOnDestroy, FOnParams);
end;

{ TSingletonLazyBind<T> }

procedure TSingletonLazyBind<T>.IncludeInjector(const AAppInjector: TAppInjector);
begin
  AAppInjector.SingletonLazy<T>(FOnCreate, FOnDestroy, FOnParams);
end;

{ TFactoryBind<T> }

procedure TFactoryBind<T>.IncludeInjector(const AAppInjector: TAppInjector);
begin
  AAppInjector.Factory<T>(FOnCreate, FOnDestroy, FOnParams);
end;

{ TSingletonInterfaceBind<T> }

procedure TSingletonInterfaceBind<I, T>.IncludeInjector(const AAppInjector: TAppInjector);
begin
  AAppInjector.SingletonInterface<I, T>('', FOnCreate, FOnDestroy, FOnParams);
end;

{ TAddInstanceBind<T> }

constructor TAddInstanceBind<T>.Create(const AInstance: TObject);
begin
  FAddInstance := Ainstance;
end;

procedure TAddInstanceBind<T>.IncludeInjector(const AAppInjector: TAppInjector);
begin
  AAppInjector.AddInstance<T>(FAddInstance);
end;

end.







