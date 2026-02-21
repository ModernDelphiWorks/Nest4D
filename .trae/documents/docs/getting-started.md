# Guia de Início Rápido - Nest4D

Bem-vindo ao Nest4D! Este guia irá ajudá-lo a criar sua primeira aplicação web com o framework em poucos minutos.

## 📋 Pré-requisitos

- **Delphi**: Versão 10.3 Rio ou superior
- **Horse Framework**: Última versão disponível
- **Git**: Para clonar o repositório
- **Conhecimento básico**: Delphi/Object Pascal e conceitos de API REST

## 🚀 Instalação

### 1. Clone o Repositório

```bash
git clone https://github.com/nest4d/nest4d.git
cd nest4d
```

### 2. Configuração do Ambiente

1. Abra o Delphi IDE
2. Adicione o caminho do Nest4D ao **Library Path**:
   - Tools → Options → Environment Options → Delphi Options → Library
   - Adicione: `$(ProjectDir)\Source`

### 3. Dependências

Certifique-se de ter o Horse Framework instalado:

```bash
# Via Boss (recomendado)
boss install horse

# Ou clone manualmente
git clone https://github.com/HashLoad/horse.git
```

## 🏗️ Primeira Aplicação

### 1. Projeto Básico

Crie um novo projeto console e adicione o código:

```pascal
program BasicServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  nest4d.core.async,
  nest4d.horse.async,
  nest4d.interceptor.async;

begin
  try
    // Criar aplicação Nest4D
    TNest4D
      .Create
      .UseInterceptor<TLoggingInterceptor>
      .Get('/api/hello', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        begin
          Res.Send('{"message": "Hello from Nest4D!"}');
        end)
      .Listen(8080);
      
    WriteLn('Servidor rodando em http://localhost:8080');
    WriteLn('Pressione ENTER para parar...');
    ReadLn;
    
  except
    on E: Exception do
      WriteLn('Erro: ' + E.Message);
  end;
end.
```

### 2. Execute o Projeto

1. Compile e execute (F9)
2. Abra o navegador em `http://localhost:8080/api/hello`
3. Você deve ver: `{"message": "Hello from Nest4D!"}`

## 🏛️ Estrutura com Controllers

### 1. Criando um Controller

```pascal
// UserController.pas
unit UserController;

interface

uses
  nest4d.core.async,
  nest4d.horse.async,
  System.JSON,
  System.SysUtils;

type
  [Route('/api/users')]
  TUserController = class(TController)
  public
    [Get]
    function GetUsers(Req: THorseRequest; Res: THorseResponse): THorseResponse;
    
    [Get('/{id}')]
    function GetUser(Req: THorseRequest; Res: THorseResponse): THorseResponse;
    
    [Post]
    function CreateUser(Req: THorseRequest; Res: THorseResponse): THorseResponse;
  end;

implementation

function TUserController.GetUsers(Req: THorseRequest; Res: THorseResponse): THorseResponse;
var
  Users: TJSONArray;
  User: TJSONObject;
begin
  Users := TJSONArray.Create;
  try
    User := TJSONObject.Create;
    User.AddPair('id', '1');
    User.AddPair('name', 'João Silva');
    User.AddPair('email', 'joao@email.com');
    Users.AddElement(User);
    
    Result := Res.Send(Users.ToString);
  finally
    Users.Free;
  end;
end;

function TUserController.GetUser(Req: THorseRequest; Res: THorseResponse): THorseResponse;
var
  UserId: string;
  User: TJSONObject;
begin
  UserId := Req.Params['id'];
  
  User := TJSONObject.Create;
  try
    User.AddPair('id', UserId);
    User.AddPair('name', 'Usuário ' + UserId);
    User.AddPair('email', 'user' + UserId + '@email.com');
    
    Result := Res.Send(User.ToString);
  finally
    User.Free;
  end;
end;

function TUserController.CreateUser(Req: THorseRequest; Res: THorseResponse): THorseResponse;
var
  Body: TJSONObject;
  Response: TJSONObject;
begin
  Body := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
  try
    Response := TJSONObject.Create;
    try
      Response.AddPair('id', '123');
      Response.AddPair('name', Body.GetValue('name').Value);
      Response.AddPair('email', Body.GetValue('email').Value);
      Response.AddPair('created_at', DateTimeToStr(Now));
      
      Result := Res.Status(201).Send(Response.ToString);
    finally
      Response.Free;
    end;
  finally
    Body.Free;
  end;
end;

end.
```

### 2. Registrando o Controller

```pascal
program ServerWithController;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  nest4d.core.async,
  nest4d.horse.async,
  UserController;

begin
  try
    TNest4D
      .Create
      .UseController<TUserController>
      .Listen(8080);
      
    WriteLn('Servidor rodando em http://localhost:8080');
    WriteLn('Endpoints disponíveis:');
    WriteLn('  GET    /api/users');
    WriteLn('  GET    /api/users/{id}');
    WriteLn('  POST   /api/users');
    WriteLn('');
    WriteLn('Pressione ENTER para parar...');
    ReadLn;
    
  except
    on E: Exception do
      WriteLn('Erro: ' + E.Message);
  end;
end.
```

## 🛡️ Adicionando Resiliência

### 1. Service com Resiliência

```pascal
// UserService.pas
unit UserService;

interface

uses
  nest4d.core.async,
  nest4d.resilience.async,
  System.SysUtils,
  System.JSON;

type
  IUserService = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function GetUsers: TJSONArray;
    function GetUser(const Id: string): TJSONObject;
  end;

  [Injectable]
  TUserService = class(TInterfacedObject, IUserService)
  public
    [Retry(MaxAttempts: 3, BackoffType: Exponential)]
    [CircuitBreaker(FailureThreshold: 5, TimeoutMs: 30000)]
    [Fallback(FallbackMethod: 'GetCachedUsers')]
    function GetUsers: TJSONArray;
    
    [Cache(TTL: 300)] // 5 minutos
    function GetUser(const Id: string): TJSONObject;
    
    // Método de fallback
    function GetCachedUsers: TJSONArray;
  end;

implementation

function TUserService.GetUsers: TJSONArray;
begin
  // Simular possível falha
  if Random(10) < 2 then // 20% de chance de falha
    raise Exception.Create('Erro simulado na base de dados');
    
  Result := TJSONArray.Create;
  // Adicionar usuários...
end;

function TUserService.GetUser(const Id: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', Id);
  Result.AddPair('name', 'Usuário ' + Id);
  Result.AddPair('cached', 'false');
end;

function TUserService.GetCachedUsers: TJSONArray;
begin
  Result := TJSONArray.Create;
  // Retornar dados em cache ou dados padrão
end;

end.
```

### 2. Configuração de Resiliência

```pascal
program ResilientServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  nest4d.core.async,
  nest4d.horse.async,
  nest4d.resilience.async,
  UserController,
  UserService;

begin
  try
    TNest4D
      .Create
      .UseResilience(TResilienceConfig.HighAvailability)
      .UseCache(TCacheConfig.Default)
      .RegisterService<IUserService, TUserService>
      .UseController<TUserController>
      .Listen(8080);
      
    WriteLn('Servidor resiliente rodando em http://localhost:8080');
    WriteLn('Endpoints de monitoramento:');
    WriteLn('  GET    /health');
    WriteLn('  GET    /metrics');
    WriteLn('  GET    /resilience');
    WriteLn('');
    WriteLn('Pressione ENTER para parar...');
    ReadLn;
    
  except
    on E: Exception do
      WriteLn('Erro: ' + E.Message);
  end;
end.
```

## 🔧 Configuração Avançada

### 1. Arquivo de Configuração

Crie `config.json`:

```json
{
  "server": {
    "port": 8080,
    "host": "localhost"
  },
  "database": {
    "host": "localhost",
    "port": 5432,
    "name": "nest4d_app",
    "username": "postgres",
    "password": "password"
  },
  "cache": {
    "defaultTTL": 300,
    "maxSize": 1000
  },
  "resilience": {
    "circuitBreaker": {
      "failureThreshold": 5,
      "timeoutMs": 30000
    },
    "retry": {
      "maxAttempts": 3,
      "backoffType": "exponential"
    }
  },
  "logging": {
    "level": "info",
    "file": "logs/app.log"
  }
}
```

### 2. Carregando Configuração

```pascal
program ConfigurableServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  nest4d.core.async,
  nest4d.horse.async,
  nest4d.config;

var
  Config: TNest4DConfig;
begin
  try
    // Carregar configuração
    Config := TNest4DConfig.LoadFromFile('config.json');
    Config.EnableHotReload; // Recarregar automaticamente
    
    TNest4D
      .Create
      .UseConfig(Config)
      .UseLogging(Config.Logging)
      .UseCache(Config.Cache)
      .UseResilience(Config.Resilience)
      .Listen(Config.Server.Port);
      
    WriteLn('Servidor configurável rodando em http://localhost:' + 
            IntToStr(Config.Server.Port));
    WriteLn('Pressione ENTER para parar...');
    ReadLn;
    
  except
    on E: Exception do
      WriteLn('Erro: ' + E.Message);
  end;
end.
```

## 🧪 Testes

### 1. Teste Unitário

```pascal
// UserServiceTests.pas
unit UserServiceTests;

interface

uses
  DUnitX.TestFramework,
  nest4d.testing.core,
  UserService;

type
  [TestFixture]
  TUserServiceTests = class
  private
    FTestingModule: INest4DTestingModule;
    FUserService: IUserService;
  public
    [Setup]
    procedure Setup;
    
    [TearDown]
    procedure TearDown;
    
    [Test]
    procedure Should_Return_Users_List;
    
    [Test]
    procedure Should_Handle_Service_Failure;
  end;

implementation

procedure TUserServiceTests.Setup;
begin
  FTestingModule := TNest4DTestingCore
    .CreateTestingModule
    .RegisterService<IUserService, TUserService>
    .Build;
    
  FUserService := FTestingModule.Get<IUserService>;
end;

procedure TUserServiceTests.TearDown;
begin
  FTestingModule := nil;
end;

procedure TUserServiceTests.Should_Return_Users_List;
var
  Users: TJSONArray;
begin
  // Act
  Users := FUserService.GetUsers;
  
  // Assert
  Assert.IsNotNull(Users);
  Assert.IsTrue(Users.Count >= 0);
  
  Users.Free;
end;

procedure TUserServiceTests.Should_Handle_Service_Failure;
begin
  // Este teste verifica se o fallback funciona
  // quando o serviço principal falha
  
  // Arrange - simular falha
  // Act & Assert
  Assert.WillNotRaise(
    procedure
    var
      Users: TJSONArray;
    begin
      Users := FUserService.GetUsers;
      Users.Free;
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TUserServiceTests);

end.
```

## 📊 Monitoramento

### 1. Health Check

O Nest4D inclui endpoints de monitoramento automáticos:

- **GET /health**: Status geral da aplicação
- **GET /metrics**: Métricas Prometheus
- **GET /resilience**: Status dos circuit breakers

### 2. Métricas Customizadas

```pascal
// No seu service
uses nest4d.metrics.async;

procedure TUserService.CreateUser;
begin
  TMetrics.Counter('users_created_total').Inc;
  TMetrics.Histogram('user_creation_duration').Observe(ExecutionTime);
end;
```

## 🚀 Próximos Passos

1. **Explore a Documentação**: Leia a [documentação completa](./architecture.md)
2. **Veja Exemplos**: Confira os [exemplos práticos](./examples/)
3. **Junte-se à Comunidade**: Participe do [Discord](https://discord.gg/nest4d)
4. **Contribua**: Faça um fork no [GitHub](https://github.com/nest4d/nest4d)

## 🆘 Problemas Comuns

### Erro: "Unit not found"
- Verifique se o caminho está no Library Path
- Confirme se todas as dependências estão instaladas

### Porta já em uso
- Altere a porta no código ou configuração
- Verifique se outro processo está usando a porta

### Erro de compilação
- Verifique a versão do Delphi (mínimo 10.3)
- Confirme se o Horse Framework está instalado

## 📚 Recursos Adicionais

- [Documentação da API](./api-reference.md)
- [Exemplos Avançados](./examples/)
- [Guia de Deploy](./deployment.md)
- [FAQ](./faq.md)

---

**Parabéns!** 🎉 Você criou sua primeira aplicação Nest4D. Agora você pode explorar recursos mais avançados como microserviços, plugins personalizados e muito mais!