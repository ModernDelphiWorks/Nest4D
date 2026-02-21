# Exemplo: Sistema de Microserviços RPC

Este exemplo demonstra como implementar comunicação entre microserviços usando o sistema RPC do Nest4D com diferentes provedores (Indy, Synapse).

## 📋 Arquitetura de Microserviços

### 1. Configuração dos Serviços

```json
// config/gateway.json
{
  "service": {
    "name": "api-gateway",
    "port": 8080,
    "version": "1.0.0"
  },
  "rpc": {
    "provider": "indy",
    "timeout": 5000,
    "retries": 3
  },
  "services": {
    "user-service": {
      "host": "localhost",
      "port": 8081,
      "provider": "indy"
    },
    "order-service": {
      "host": "localhost",
      "port": 8082,
      "provider": "synapse"
    },
    "notification-service": {
      "host": "localhost",
      "port": 8083,
      "provider": "indy"
    }
  },
  "load_balancer": {
    "strategy": "round_robin",
    "health_check_interval": 30000
  }
}
```

```json
// config/user-service.json
{
  "service": {
    "name": "user-service",
    "port": 8081,
    "version": "1.0.0"
  },
  "rpc": {
    "provider": "indy",
    "max_connections": 100,
    "timeout": 3000
  },
  "database": {
    "host": "localhost",
    "port": 5432,
    "name": "users_db"
  }
}
```

### 2. Interfaces de Serviço

```pascal
// Shared\ServiceInterfaces.pas
unit ServiceInterfaces;

interface

uses
  System.SysUtils,
  System.JSON;

type
  // DTOs compartilhados
  TUserDTO = record
    ID: Integer;
    Name: string;
    Email: string;
    CreatedAt: TDateTime;
    Active: Boolean;
  end;

  TOrderDTO = record
    ID: Integer;
    UserID: Integer;
    ProductName: string;
    Quantity: Integer;
    Price: Double;
    Status: string;
    CreatedAt: TDateTime;
  end;

  TNotificationDTO = record
    ID: string;
    UserID: Integer;
    Title: string;
    Message: string;
    Type_: string;
    Sent: Boolean;
    CreatedAt: TDateTime;
  end;

  // Interfaces de serviço
  IUserService = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetUser(UserID: Integer): TUserDTO;
    function GetUsers: TArray<TUserDTO>;
    function CreateUser(const Name, Email: string): TUserDTO;
    function UpdateUser(UserID: Integer; const Name, Email: string): TUserDTO;
    function DeleteUser(UserID: Integer): Boolean;
    function ValidateUser(const Email, Password: string): Boolean;
  end;

  IOrderService = interface
    ['{B2C3D4E5-F6G7-8901-BCDE-F23456789012}']
    function GetOrder(OrderID: Integer): TOrderDTO;
    function GetOrdersByUser(UserID: Integer): TArray<TOrderDTO>;
    function CreateOrder(UserID: Integer; const ProductName: string; Quantity: Integer; Price: Double): TOrderDTO;
    function UpdateOrderStatus(OrderID: Integer; const Status: string): Boolean;
    function CancelOrder(OrderID: Integer): Boolean;
  end;

  INotificationService = interface
    ['{C3D4E5F6-G7H8-9012-CDEF-345678901234}']
    function SendNotification(UserID: Integer; const Title, Message, Type_: string): TNotificationDTO;
    function GetNotifications(UserID: Integer): TArray<TNotificationDTO>;
    function MarkAsRead(NotificationID: string): Boolean;
    function GetUnreadCount(UserID: Integer): Integer;
  end;

  // Utilitários de conversão
  TDTOHelper = class
  public
    class function UserToJSON(const User: TUserDTO): TJSONObject;
    class function JSONToUser(JSON: TJSONObject): TUserDTO;
    class function OrderToJSON(const Order: TOrderDTO): TJSONObject;
    class function JSONToOrder(JSON: TJSONObject): TOrderDTO;
    class function NotificationToJSON(const Notification: TNotificationDTO): TJSONObject;
    class function JSONToNotification(JSON: TJSONObject): TNotificationDTO;
  end;

implementation

class function TDTOHelper.UserToJSON(const User: TUserDTO): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', User.ID);
  Result.AddPair('name', User.Name);
  Result.AddPair('email', User.Email);
  Result.AddPair('created_at', DateToISO8601(User.CreatedAt));
  Result.AddPair('active', User.Active);
end;

class function TDTOHelper.JSONToUser(JSON: TJSONObject): TUserDTO;
begin
  Result.ID := JSON.GetValue<Integer>('id');
  Result.Name := JSON.GetValue<string>('name');
  Result.Email := JSON.GetValue<string>('email');
  Result.CreatedAt := ISO8601ToDate(JSON.GetValue<string>('created_at'));
  Result.Active := JSON.GetValue<Boolean>('active');
end;

class function TDTOHelper.OrderToJSON(const Order: TOrderDTO): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', Order.ID);
  Result.AddPair('user_id', Order.UserID);
  Result.AddPair('product_name', Order.ProductName);
  Result.AddPair('quantity', Order.Quantity);
  Result.AddPair('price', Order.Price);
  Result.AddPair('status', Order.Status);
  Result.AddPair('created_at', DateToISO8601(Order.CreatedAt));
end;

class function TDTOHelper.JSONToOrder(JSON: TJSONObject): TOrderDTO;
begin
  Result.ID := JSON.GetValue<Integer>('id');
  Result.UserID := JSON.GetValue<Integer>('user_id');
  Result.ProductName := JSON.GetValue<string>('product_name');
  Result.Quantity := JSON.GetValue<Integer>('quantity');
  Result.Price := JSON.GetValue<Double>('price');
  Result.Status := JSON.GetValue<string>('status');
  Result.CreatedAt := ISO8601ToDate(JSON.GetValue<string>('created_at'));
end;

class function TDTOHelper.NotificationToJSON(const Notification: TNotificationDTO): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', Notification.ID);
  Result.AddPair('user_id', Notification.UserID);
  Result.AddPair('title', Notification.Title);
  Result.AddPair('message', Notification.Message);
  Result.AddPair('type', Notification.Type_);
  Result.AddPair('sent', Notification.Sent);
  Result.AddPair('created_at', DateToISO8601(Notification.CreatedAt));
end;

class function TDTOHelper.JSONToNotification(JSON: TJSONObject): TNotificationDTO;
begin
  Result.ID := JSON.GetValue<string>('id');
  Result.UserID := JSON.GetValue<Integer>('user_id');
  Result.Title := JSON.GetValue<string>('title');
  Result.Message := JSON.GetValue<string>('message');
  Result.Type_ := JSON.GetValue<string>('type');
  Result.Sent := JSON.GetValue<Boolean>('sent');
  Result.CreatedAt := ISO8601ToDate(JSON.GetValue<string>('created_at'));
end;

end.
```

## 🔧 Implementação dos Serviços

### 1. User Service

```pascal
// UserService\UserServiceImpl.pas
unit UserServiceImpl;

interface

uses
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  nest4d.rpc.server,
  nest4d.rpc.client,
  nest4d.server.indy,
  ServiceInterfaces;

type
  TUserServiceImpl = class(TInterfacedObject, IUserService)
  private
    FUsers: TDictionary<Integer, TUserDTO>;
    FNextID: Integer;
    
    procedure InitializeSampleData;
  public
    constructor Create;
    destructor Destroy; override;
    
    // IUserService
    function GetUser(UserID: Integer): TUserDTO;
    function GetUsers: TArray<TUserDTO>;
    function CreateUser(const Name, Email: string): TUserDTO;
    function UpdateUser(UserID: Integer; const Name, Email: string): TUserDTO;
    function DeleteUser(UserID: Integer): Boolean;
    function ValidateUser(const Email, Password: string): Boolean;
  end;

  TUserServiceServer = class
  private
    FRPCServer: TRPCProviderServerIndy;
    FUserService: IUserService;
    
    procedure RegisterRPCHandlers;
    procedure HandleGetUser(const Request: string; out Response: string);
    procedure HandleGetUsers(const Request: string; out Response: string);
    procedure HandleCreateUser(const Request: string; out Response: string);
    procedure HandleUpdateUser(const Request: string; out Response: string);
    procedure HandleDeleteUser(const Request: string; out Response: string);
    procedure HandleValidateUser(const Request: string; out Response: string);
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Start(Port: Integer);
    procedure Stop;
  end;

implementation

// TUserServiceImpl

constructor TUserServiceImpl.Create;
begin
  inherited;
  FUsers := TDictionary<Integer, TUserDTO>.Create;
  FNextID := 1;
  InitializeSampleData;
end;

destructor TUserServiceImpl.Destroy;
begin
  FUsers.Free;
  inherited;
end;

procedure TUserServiceImpl.InitializeSampleData;
var
  User: TUserDTO;
begin
  // Usuário 1
  User.ID := FNextID;
  User.Name := 'João Silva';
  User.Email := 'joao@email.com';
  User.CreatedAt := Now;
  User.Active := True;
  FUsers.Add(FNextID, User);
  Inc(FNextID);
  
  // Usuário 2
  User.ID := FNextID;
  User.Name := 'Maria Santos';
  User.Email := 'maria@email.com';
  User.CreatedAt := Now;
  User.Active := True;
  FUsers.Add(FNextID, User);
  Inc(FNextID);
end;

function TUserServiceImpl.GetUser(UserID: Integer): TUserDTO;
begin
  if not FUsers.TryGetValue(UserID, Result) then
    raise Exception.CreateFmt('User not found: %d', [UserID]);
end;

function TUserServiceImpl.GetUsers: TArray<TUserDTO>;
var
  User: TUserDTO;
begin
  SetLength(Result, FUsers.Count);
  var I := 0;
  for User in FUsers.Values do
  begin
    Result[I] := User;
    Inc(I);
  end;
end;

function TUserServiceImpl.CreateUser(const Name, Email: string): TUserDTO;
begin
  Result.ID := FNextID;
  Result.Name := Name;
  Result.Email := Email;
  Result.CreatedAt := Now;
  Result.Active := True;
  
  FUsers.Add(FNextID, Result);
  Inc(FNextID);
end;

function TUserServiceImpl.UpdateUser(UserID: Integer; const Name, Email: string): TUserDTO;
begin
  if not FUsers.TryGetValue(UserID, Result) then
    raise Exception.CreateFmt('User not found: %d', [UserID]);
    
  Result.Name := Name;
  Result.Email := Email;
  FUsers.AddOrSetValue(UserID, Result);
end;

function TUserServiceImpl.DeleteUser(UserID: Integer): Boolean;
begin
  Result := FUsers.Remove(UserID);
end;

function TUserServiceImpl.ValidateUser(const Email, Password: string): Boolean;
var
  User: TUserDTO;
begin
  // Simulação simples de validação
  for User in FUsers.Values do
  begin
    if SameText(User.Email, Email) and User.Active then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

// TUserServiceServer

constructor TUserServiceServer.Create;
begin
  inherited;
  FUserService := TUserServiceImpl.Create;
  FRPCServer := TRPCProviderServerIndy.Create;
  RegisterRPCHandlers;
end;

destructor TUserServiceServer.Destroy;
begin
  FRPCServer.Free;
  inherited;
end;

procedure TUserServiceServer.RegisterRPCHandlers;
begin
  FRPCServer.PublishRPC('GetUser', HandleGetUser);
  FRPCServer.PublishRPC('GetUsers', HandleGetUsers);
  FRPCServer.PublishRPC('CreateUser', HandleCreateUser);
  FRPCServer.PublishRPC('UpdateUser', HandleUpdateUser);
  FRPCServer.PublishRPC('DeleteUser', HandleDeleteUser);
  FRPCServer.PublishRPC('ValidateUser', HandleValidateUser);
end;

procedure TUserServiceServer.HandleGetUser(const Request: string; out Response: string);
var
  RequestJSON: TJSONObject;
  UserID: Integer;
  User: TUserDTO;
  ResponseJSON: TJSONObject;
begin
  try
    RequestJSON := TJSONObject.ParseJSONValue(Request) as TJSONObject;
    try
      UserID := RequestJSON.GetValue<Integer>('user_id');
      User := FUserService.GetUser(UserID);
      
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', True);
        ResponseJSON.AddPair('data', TDTOHelper.UserToJSON(User));
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    finally
      RequestJSON.Free;
    end;
  except
    on E: Exception do
    begin
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', False);
        ResponseJSON.AddPair('error', E.Message);
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    end;
  end;
end;

procedure TUserServiceServer.HandleGetUsers(const Request: string; out Response: string);
var
  Users: TArray<TUserDTO>;
  ResponseJSON: TJSONObject;
  UsersArray: TJSONArray;
  User: TUserDTO;
begin
  try
    Users := FUserService.GetUsers;
    
    UsersArray := TJSONArray.Create;
    for User in Users do
      UsersArray.AddElement(TDTOHelper.UserToJSON(User));
    
    ResponseJSON := TJSONObject.Create;
    try
      ResponseJSON.AddPair('success', True);
      ResponseJSON.AddPair('data', UsersArray);
      Response := ResponseJSON.ToString;
    finally
      ResponseJSON.Free;
    end;
  except
    on E: Exception do
    begin
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', False);
        ResponseJSON.AddPair('error', E.Message);
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    end;
  end;
end;

procedure TUserServiceServer.HandleCreateUser(const Request: string; out Response: string);
var
  RequestJSON: TJSONObject;
  Name, Email: string;
  User: TUserDTO;
  ResponseJSON: TJSONObject;
begin
  try
    RequestJSON := TJSONObject.ParseJSONValue(Request) as TJSONObject;
    try
      Name := RequestJSON.GetValue<string>('name');
      Email := RequestJSON.GetValue<string>('email');
      User := FUserService.CreateUser(Name, Email);
      
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', True);
        ResponseJSON.AddPair('data', TDTOHelper.UserToJSON(User));
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    finally
      RequestJSON.Free;
    end;
  except
    on E: Exception do
    begin
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', False);
        ResponseJSON.AddPair('error', E.Message);
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    end;
  end;
end;

procedure TUserServiceServer.HandleUpdateUser(const Request: string; out Response: string);
var
  RequestJSON: TJSONObject;
  UserID: Integer;
  Name, Email: string;
  User: TUserDTO;
  ResponseJSON: TJSONObject;
begin
  try
    RequestJSON := TJSONObject.ParseJSONValue(Request) as TJSONObject;
    try
      UserID := RequestJSON.GetValue<Integer>('user_id');
      Name := RequestJSON.GetValue<string>('name');
      Email := RequestJSON.GetValue<string>('email');
      User := FUserService.UpdateUser(UserID, Name, Email);
      
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', True);
        ResponseJSON.AddPair('data', TDTOHelper.UserToJSON(User));
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    finally
      RequestJSON.Free;
    end;
  except
    on E: Exception do
    begin
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', False);
        ResponseJSON.AddPair('error', E.Message);
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    end;
  end;
end;

procedure TUserServiceServer.HandleDeleteUser(const Request: string; out Response: string);
var
  RequestJSON: TJSONObject;
  UserID: Integer;
  Success: Boolean;
  ResponseJSON: TJSONObject;
begin
  try
    RequestJSON := TJSONObject.ParseJSONValue(Request) as TJSONObject;
    try
      UserID := RequestJSON.GetValue<Integer>('user_id');
      Success := FUserService.DeleteUser(UserID);
      
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', Success);
        if not Success then
          ResponseJSON.AddPair('error', 'User not found');
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    finally
      RequestJSON.Free;
    end;
  except
    on E: Exception do
    begin
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', False);
        ResponseJSON.AddPair('error', E.Message);
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    end;
  end;
end;

procedure TUserServiceServer.HandleValidateUser(const Request: string; out Response: string);
var
  RequestJSON: TJSONObject;
  Email, Password: string;
  Valid: Boolean;
  ResponseJSON: TJSONObject;
begin
  try
    RequestJSON := TJSONObject.ParseJSONValue(Request) as TJSONObject;
    try
      Email := RequestJSON.GetValue<string>('email');
      Password := RequestJSON.GetValue<string>('password');
      Valid := FUserService.ValidateUser(Email, Password);
      
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', True);
        ResponseJSON.AddPair('valid', Valid);
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    finally
      RequestJSON.Free;
    end;
  except
    on E: Exception do
    begin
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.AddPair('success', False);
        ResponseJSON.AddPair('error', E.Message);
        Response := ResponseJSON.ToString;
      finally
        ResponseJSON.Free;
      end;
    end;
  end;
end;

procedure TUserServiceServer.Start(Port: Integer);
begin
  FRPCServer.Start(Port);
  WriteLn(Format('👤 User Service started on port %d', [Port]));
end;

procedure TUserServiceServer.Stop;
begin
  FRPCServer.Stop;
  WriteLn('👤 User Service stopped');
end;

end.
```

### 2. API Gateway

```pascal
// Gateway\APIGateway.pas
unit APIGateway;

interface

uses
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  Horse,
  nest4d.core.async,
  nest4d.horse.async,
  nest4d.rpc.client,
  nest4d.client.indy,
  nest4d.client.synapse,
  ServiceInterfaces;

type
  TServiceClient = record
    Name: string;
    Host: string;
    Port: Integer;
    Provider: string;
    Client: IRPCProviderClient;
    Available: Boolean;
    LastCheck: TDateTime;
  end;

  TAPIGateway = class
  private
    FServices: TDictionary<string, TServiceClient>;
    FLoadBalancer: TDictionary<string, Integer>; // Round-robin counters
    
    procedure InitializeServices;
    procedure RegisterRoutes;
    function GetServiceClient(const ServiceName: string): IRPCProviderClient;
    function CallService(const ServiceName, Method: string; const Request: TJSONObject): TJSONObject;
    
    // Route handlers
    procedure HandleGetUsers(Req: THorseRequest; Res: THorseResponse);
    procedure HandleGetUser(Req: THorseRequest; Res: THorseResponse);
    procedure HandleCreateUser(Req: THorseRequest; Res: THorseResponse);
    procedure HandleUpdateUser(Req: THorseRequest; Res: THorseResponse);
    procedure HandleDeleteUser(Req: THorseRequest; Res: THorseResponse);
    
    procedure HandleGetOrders(Req: THorseRequest; Res: THorseResponse);
    procedure HandleCreateOrder(Req: THorseRequest; Res: THorseResponse);
    
    procedure HandleSendNotification(Req: THorseRequest; Res: THorseResponse);
    procedure HandleGetNotifications(Req: THorseRequest; Res: THorseResponse);
    
    // Health check
    procedure HandleHealthCheck(Req: THorseRequest; Res: THorseResponse);
    procedure HandleServiceStatus(Req: THorseRequest; Res: THorseResponse);
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Start(Port: Integer);
    procedure Stop;
    procedure CheckServiceHealth;
  end;

implementation

constructor TAPIGateway.Create;
begin
  inherited;
  FServices := TDictionary<string, TServiceClient>.Create;
  FLoadBalancer := TDictionary<string, Integer>.Create;
  InitializeServices;
  RegisterRoutes;
end;

destructor TAPIGateway.Destroy;
var
  ServiceClient: TServiceClient;
begin
  for ServiceClient in FServices.Values do
    ServiceClient.Client := nil;
    
  FServices.Free;
  FLoadBalancer.Free;
  inherited;
end;

procedure TAPIGateway.InitializeServices;
var
  ServiceClient: TServiceClient;
begin
  // User Service
  ServiceClient.Name := 'user-service';
  ServiceClient.Host := 'localhost';
  ServiceClient.Port := 8081;
  ServiceClient.Provider := 'indy';
  ServiceClient.Client := TRPCProviderClientIndy.Create;
  ServiceClient.Available := True;
  ServiceClient.LastCheck := Now;
  FServices.Add('user-service', ServiceClient);
  FLoadBalancer.Add('user-service', 0);
  
  // Order Service
  ServiceClient.Name := 'order-service';
  ServiceClient.Host := 'localhost';
  ServiceClient.Port := 8082;
  ServiceClient.Provider := 'synapse';
  ServiceClient.Client := TRPCProviderClientSynapse.Create;
  ServiceClient.Available := True;
  ServiceClient.LastCheck := Now;
  FServices.Add('order-service', ServiceClient);
  FLoadBalancer.Add('order-service', 0);
  
  // Notification Service
  ServiceClient.Name := 'notification-service';
  ServiceClient.Host := 'localhost';
  ServiceClient.Port := 8083;
  ServiceClient.Provider := 'indy';
  ServiceClient.Client := TRPCProviderClientIndy.Create;
  ServiceClient.Available := True;
  ServiceClient.LastCheck := Now;
  FServices.Add('notification-service', ServiceClient);
  FLoadBalancer.Add('notification-service', 0);
end;

procedure TAPIGateway.RegisterRoutes;
begin
  TNest4D
    .Create
    
    // User routes
    .Get('/api/users', HandleGetUsers)
    .Get('/api/users/:id', HandleGetUser)
    .Post('/api/users', HandleCreateUser)
    .Put('/api/users/:id', HandleUpdateUser)
    .Delete('/api/users/:id', HandleDeleteUser)
    
    // Order routes
    .Get('/api/users/:id/orders', HandleGetOrders)
    .Post('/api/orders', HandleCreateOrder)
    
    // Notification routes
    .Post('/api/notifications', HandleSendNotification)
    .Get('/api/users/:id/notifications', HandleGetNotifications)
    
    // Health and monitoring
    .Get('/health', HandleHealthCheck)
    .Get('/services/status', HandleServiceStatus)
    
    // CORS middleware
    .Use(procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        Res.AddHeader('Access-Control-Allow-Origin', '*');
        Res.AddHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
        Res.AddHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
        
        if Req.MethodType = mtOptions then
          Res.Status(200).Send('')
        else
          Next();
      end);
end;

function TAPIGateway.GetServiceClient(const ServiceName: string): IRPCProviderClient;
var
  ServiceClient: TServiceClient;
begin
  if FServices.TryGetValue(ServiceName, ServiceClient) and ServiceClient.Available then
    Result := ServiceClient.Client
  else
    raise Exception.CreateFmt('Service not available: %s', [ServiceName]);
end;

function TAPIGateway.CallService(const ServiceName, Method: string; const Request: TJSONObject): TJSONObject;
var
  Client: IRPCProviderClient;
  ServiceClient: TServiceClient;
  RequestStr, ResponseStr: string;
begin
  Client := GetServiceClient(ServiceName);
  ServiceClient := FServices[ServiceName];
  
  RequestStr := Request.ToString;
  
  try
    ResponseStr := Client.ExecuteRPC(ServiceClient.Host, ServiceClient.Port, Method, RequestStr);
    Result := TJSONObject.ParseJSONValue(ResponseStr) as TJSONObject;
    
    if not Assigned(Result) then
      raise Exception.Create('Invalid response from service');
      
  except
    on E: Exception do
    begin
      // Mark service as unavailable
      ServiceClient.Available := False;
      FServices.AddOrSetValue(ServiceName, ServiceClient);
      
      WriteLn(Format('❌ Service call failed: %s.%s - %s', [ServiceName, Method, E.Message]));
      raise;
    end;
  end;
end;

procedure TAPIGateway.HandleGetUsers(Req: THorseRequest; Res: THorseResponse);
var
  Request: TJSONObject;
  Response: TJSONObject;
begin
  try
    Request := TJSONObject.Create;
    try
      Response := CallService('user-service', 'GetUsers', Request);
      try
        if Response.GetValue<Boolean>('success') then
          Res.Send(Response.GetValue('data').ToString)
        else
          Res.Status(500).Send(Response.ToString);
      finally
        Response.Free;
      end;
    finally
      Request.Free;
    end;
  except
    on E: Exception do
      Res.Status(503).Send(Format('{"error": "Service unavailable: %s"}', [E.Message]));
  end;
end;

procedure TAPIGateway.HandleGetUser(Req: THorseRequest; Res: THorseResponse);
var
  UserID: Integer;
  Request: TJSONObject;
  Response: TJSONObject;
begin
  try
    UserID := StrToInt(Req.Params['id']);
    
    Request := TJSONObject.Create;
    try
      Request.AddPair('user_id', UserID);
      
      Response := CallService('user-service', 'GetUser', Request);
      try
        if Response.GetValue<Boolean>('success') then
          Res.Send(Response.GetValue('data').ToString)
        else
          Res.Status(404).Send(Response.ToString);
      finally
        Response.Free;
      end;
    finally
      Request.Free;
    end;
  except
    on E: EConvertError do
      Res.Status(400).Send('{"error": "Invalid user ID"}');
    on E: Exception do
      Res.Status(503).Send(Format('{"error": "Service unavailable: %s"}', [E.Message]));
  end;
end;

procedure TAPIGateway.HandleCreateUser(Req: THorseRequest; Res: THorseResponse);
var
  RequestBody: TJSONObject;
  Request: TJSONObject;
  Response: TJSONObject;
  Name, Email: string;
begin
  try
    RequestBody := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
    if not Assigned(RequestBody) then
    begin
      Res.Status(400).Send('{"error": "Invalid JSON body"}');
      Exit;
    end;
    
    try
      Name := RequestBody.GetValue<string>('name');
      Email := RequestBody.GetValue<string>('email');
      
      Request := TJSONObject.Create;
      try
        Request.AddPair('name', Name);
        Request.AddPair('email', Email);
        
        Response := CallService('user-service', 'CreateUser', Request);
        try
          if Response.GetValue<Boolean>('success') then
            Res.Status(201).Send(Response.GetValue('data').ToString)
          else
            Res.Status(400).Send(Response.ToString);
        finally
          Response.Free;
        end;
      finally
        Request.Free;
      end;
    finally
      RequestBody.Free;
    end;
  except
    on E: Exception do
      Res.Status(503).Send(Format('{"error": "Service unavailable: %s"}', [E.Message]));
  end;
end;

procedure TAPIGateway.HandleUpdateUser(Req: THorseRequest; Res: THorseResponse);
var
  UserID: Integer;
  RequestBody: TJSONObject;
  Request: TJSONObject;
  Response: TJSONObject;
  Name, Email: string;
begin
  try
    UserID := StrToInt(Req.Params['id']);
    
    RequestBody := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
    if not Assigned(RequestBody) then
    begin
      Res.Status(400).Send('{"error": "Invalid JSON body"}');
      Exit;
    end;
    
    try
      Name := RequestBody.GetValue<string>('name');
      Email := RequestBody.GetValue<string>('email');
      
      Request := TJSONObject.Create;
      try
        Request.AddPair('user_id', UserID);
        Request.AddPair('name', Name);
        Request.AddPair('email', Email);
        
        Response := CallService('user-service', 'UpdateUser', Request);
        try
          if Response.GetValue<Boolean>('success') then
            Res.Send(Response.GetValue('data').ToString)
          else
            Res.Status(404).Send(Response.ToString);
        finally
          Response.Free;
        end;
      finally
        Request.Free;
      end;
    finally
      RequestBody.Free;
    end;
  except
    on E: EConvertError do
      Res.Status(400).Send('{"error": "Invalid user ID"}');
    on E: Exception do
      Res.Status(503).Send(Format('{"error": "Service unavailable: %s"}', [E.Message]));
  end;
end;

procedure TAPIGateway.HandleDeleteUser(Req: THorseRequest; Res: THorseResponse);
var
  UserID: Integer;
  Request: TJSONObject;
  Response: TJSONObject;
begin
  try
    UserID := StrToInt(Req.Params['id']);
    
    Request := TJSONObject.Create;
    try
      Request.AddPair('user_id', UserID);
      
      Response := CallService('user-service', 'DeleteUser', Request);
      try
        if Response.GetValue<Boolean>('success') then
          Res.Status(204).Send('')
        else
          Res.Status(404).Send(Response.ToString);
      finally
        Response.Free;
      end;
    finally
      Request.Free;
    end;
  except
    on E: EConvertError do
      Res.Status(400).Send('{"error": "Invalid user ID"}');
    on E: Exception do
      Res.Status(503).Send(Format('{"error": "Service unavailable: %s"}', [E.Message]));
  end;
end;

procedure TAPIGateway.HandleGetOrders(Req: THorseRequest; Res: THorseResponse);
var
  UserID: Integer;
  Request: TJSONObject;
  Response: TJSONObject;
begin
  try
    UserID := StrToInt(Req.Params['id']);
    
    Request := TJSONObject.Create;
    try
      Request.AddPair('user_id', UserID);
      
      Response := CallService('order-service', 'GetOrdersByUser', Request);
      try
        if Response.GetValue<Boolean>('success') then
          Res.Send(Response.GetValue('data').ToString)
        else
          Res.Status(500).Send(Response.ToString);
      finally
        Response.Free;
      end;
    finally
      Request.Free;
    end;
  except
    on E: EConvertError do
      Res.Status(400).Send('{"error": "Invalid user ID"}');
    on E: Exception do
      Res.Status(503).Send(Format('{"error": "Service unavailable: %s"}', [E.Message]));
  end;
end;

procedure TAPIGateway.HandleCreateOrder(Req: THorseRequest; Res: THorseResponse);
var
  RequestBody: TJSONObject;
  Request: TJSONObject;
  Response: TJSONObject;
  UserID, Quantity: Integer;
  ProductName: string;
  Price: Double;
begin
  try
    RequestBody := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
    if not Assigned(RequestBody) then
    begin
      Res.Status(400).Send('{"error": "Invalid JSON body"}');
      Exit;
    end;
    
    try
      UserID := RequestBody.GetValue<Integer>('user_id');
      ProductName := RequestBody.GetValue<string>('product_name');
      Quantity := RequestBody.GetValue<Integer>('quantity');
      Price := RequestBody.GetValue<Double>('price');
      
      Request := TJSONObject.Create;
      try
        Request.AddPair('user_id', UserID);
        Request.AddPair('product_name', ProductName);
        Request.AddPair('quantity', Quantity);
        Request.AddPair('price', Price);
        
        Response := CallService('order-service', 'CreateOrder', Request);
        try
          if Response.GetValue<Boolean>('success') then
            Res.Status(201).Send(Response.GetValue('data').ToString)
          else
            Res.Status(400).Send(Response.ToString);
        finally
          Response.Free;
        end;
      finally
        Request.Free;
      end;
    finally
      RequestBody.Free;
    end;
  except
    on E: Exception do
      Res.Status(503).Send(Format('{"error": "Service unavailable: %s"}', [E.Message]));
  end;
end;

procedure TAPIGateway.HandleSendNotification(Req: THorseRequest; Res: THorseResponse);
var
  RequestBody: TJSONObject;
  Request: TJSONObject;
  Response: TJSONObject;
  UserID: Integer;
  Title, Message, Type_: string;
begin
  try
    RequestBody := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
    if not Assigned(RequestBody) then
    begin
      Res.Status(400).Send('{"error": "Invalid JSON body"}');
      Exit;
    end;
    
    try
      UserID := RequestBody.GetValue<Integer>('user_id');
      Title := RequestBody.GetValue<string>('title');
      Message := RequestBody.GetValue<string>('message');
      Type_ := RequestBody.GetValue<string>('type');
      
      Request := TJSONObject.Create;
      try
        Request.AddPair('user_id', UserID);
        Request.AddPair('title', Title);
        Request.AddPair('message', Message);
        Request.AddPair('type', Type_);
        
        Response := CallService('notification-service', 'SendNotification', Request);
        try
          if Response.GetValue<Boolean>('success') then
            Res.Status(201).Send(Response.GetValue('data').ToString)
          else
            Res.Status(400).Send(Response.ToString);
        finally
          Response.Free;
        end;
      finally
        Request.Free;
      end;
    finally
      RequestBody.Free;
    end;
  except
    on E: Exception do
      Res.Status(503).Send(Format('{"error": "Service unavailable: %s"}', [E.Message]));
  end;
end;

procedure TAPIGateway.HandleGetNotifications(Req: THorseRequest; Res: THorseResponse);
var
  UserID: Integer;
  Request: TJSONObject;
  Response: TJSONObject;
begin
  try
    UserID := StrToInt(Req.Params['id']);
    
    Request := TJSONObject.Create;
    try
      Request.AddPair('user_id', UserID);
      
      Response := CallService('notification-service', 'GetNotifications', Request);
      try
        if Response.GetValue<Boolean>('success') then
          Res.Send(Response.GetValue('data').ToString)
        else
          Res.Status(500).Send(Response.ToString);
      finally
        Response.Free;
      end;
    finally
      Request.Free;
    end;
  except
    on E: EConvertError do
      Res.Status(400).Send('{"error": "Invalid user ID"}');
    on E: Exception do
      Res.Status(503).Send(Format('{"error": "Service unavailable: %s"}', [E.Message]));
  end;
end;

procedure TAPIGateway.HandleHealthCheck(Req: THorseRequest; Res: THorseResponse);
var
  Health: TJSONObject;
  Services: TJSONObject;
  ServiceClient: TServiceClient;
  ServiceName: string;
begin
  Health := TJSONObject.Create;
  Services := TJSONObject.Create;
  
  try
    CheckServiceHealth;
    
    for ServiceName in FServices.Keys do
    begin
      ServiceClient := FServices[ServiceName];
      Services.AddPair(ServiceName, ServiceClient.Available);
    end;
    
    Health.AddPair('status', 'healthy');
    Health.AddPair('timestamp', DateTimeToStr(Now));
    Health.AddPair('services', Services);
    
    Res.Send(Health.ToString);
  finally
    Health.Free;
  end;
end;

procedure TAPIGateway.HandleServiceStatus(Req: THorseRequest; Res: THorseResponse);
var
  Status: TJSONObject;
  ServicesArray: TJSONArray;
  ServiceObj: TJSONObject;
  ServiceClient: TServiceClient;
  ServiceName: string;
begin
  Status := TJSONObject.Create;
  ServicesArray := TJSONArray.Create;
  
  try
    for ServiceName in FServices.Keys do
    begin
      ServiceClient := FServices[ServiceName];
      ServiceObj := TJSONObject.Create;
      ServiceObj.AddPair('name', ServiceClient.Name);
      ServiceObj.AddPair('host', ServiceClient.Host);
      ServiceObj.AddPair('port', ServiceClient.Port);
      ServiceObj.AddPair('provider', ServiceClient.Provider);
      ServiceObj.AddPair('available', ServiceClient.Available);
      ServiceObj.AddPair('last_check', DateTimeToStr(ServiceClient.LastCheck));
      ServicesArray.AddElement(ServiceObj);
    end;
    
    Status.AddPair('gateway', 'API Gateway');
    Status.AddPair('version', '1.0.0');
    Status.AddPair('timestamp', DateTimeToStr(Now));
    Status.AddPair('services', ServicesArray);
    
    Res.Send(Status.ToString);
  finally
    Status.Free;
  end;
end;

procedure TAPIGateway.Start(Port: Integer);
begin
  TNest4D.Listen(Port);
  WriteLn(Format('🌐 API Gateway started on port %d', [Port]));
end;

procedure TAPIGateway.Stop;
begin
  TNest4D.Stop;
  WriteLn('🌐 API Gateway stopped');
end;

procedure TAPIGateway.CheckServiceHealth;
var
  ServiceName: string;
  ServiceClient: TServiceClient;
  Request: TJSONObject;
  Response: TJSONObject;
begin
  for ServiceName in FServices.Keys do
  begin
    ServiceClient := FServices[ServiceName];
    
    try
      Request := TJSONObject.Create;
      try
        // Tentar uma chamada simples para verificar se o serviço está ativo
        Response := CallService(ServiceName, 'HealthCheck', Request);
        try
          ServiceClient.Available := True;
          ServiceClient.LastCheck := Now;
        finally
          Response.Free;
        end;
      finally
        Request.Free;
      end;
    except
      ServiceClient.Available := False;
      ServiceClient.LastCheck := Now;
    end;
    
    FServices.AddOrSetValue(ServiceName, ServiceClient);
  end;
end;

end.
```

### 3. Programa Principal do Gateway

```pascal
program MicroservicesGateway;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  APIGateway;

var
  Gateway: TAPIGateway;

begin
  try
    WriteLn('=== Nest4D Microservices Gateway ===');
    WriteLn('');
    
    Gateway := TAPIGateway.Create;
    try
      Gateway.Start(8080);
      
      WriteLn('🌐 API Gateway rodando em http://localhost:8080');
      WriteLn('');
      WriteLn('📡 Endpoints disponíveis:');
      WriteLn('  GET    /api/users              - Listar usuários');
      WriteLn('  GET    /api/users/:id          - Buscar usuário');
      WriteLn('  POST   /api/users              - Criar usuário');
      WriteLn('  PUT    /api/users/:id          - Atualizar usuário');
      WriteLn('  DELETE /api/users/:id          - Deletar usuário');
      WriteLn('  GET    /api/users/:id/orders   - Pedidos do usuário');
      WriteLn('  POST   /api/orders             - Criar pedido');
      WriteLn('  POST   /api/notifications      - Enviar notificação');
      WriteLn('  GET    /api/users/:id/notifications - Notificações do usuário');
      WriteLn('');
      WriteLn('🔧 Monitoramento:');
      WriteLn('  GET    /health                 - Status do gateway');
      WriteLn('  GET    /services/status        - Status dos serviços');
      WriteLn('');
      WriteLn('🏗️  Serviços conectados:');
      WriteLn('  • User Service (Indy) - localhost:8081');
      WriteLn('  • Order Service (Synapse) - localhost:8082');
      WriteLn('  • Notification Service (Indy) - localhost:8083');
      WriteLn('');
      WriteLn('Pressione ENTER para parar o gateway...');
      ReadLn;
      
      Gateway.Stop;
    finally
      Gateway.Free;
    end;
    
  except
    on E: Exception do
    begin
      WriteLn('❌ Erro: ' + E.Message);
      WriteLn('Pressione ENTER para sair...');
      ReadLn;
    end;
  end;
end.
```

### 4. Programa Principal do User Service

```pascal
program UserServiceApp;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  UserServiceImpl;

var
  UserServer: TUserServiceServer;

begin
  try
    WriteLn('=== Nest4D User Service ===');
    WriteLn('');
    
    UserServer := TUserServiceServer.Create;
    try
      UserServer.Start(8081);
      
      WriteLn('📡 RPC Methods disponíveis:');
      WriteLn('  • GetUser');
      WriteLn('  • GetUsers');
      WriteLn('  • CreateUser');
      WriteLn('  • UpdateUser');
      WriteLn('  • DeleteUser');
      WriteLn('  • ValidateUser');
      WriteLn('');
      WriteLn('Pressione ENTER para parar o serviço...');
      ReadLn;
      
      UserServer.Stop;
    finally
      UserServer.Free;
    end;
    
  except
    on E: Exception do
    begin
      WriteLn('❌ Erro: ' + E.Message);
      WriteLn('Pressione ENTER para sair...');
      ReadLn;
    end;
  end;
end.
```

## 🧪 Testando os Microserviços

### 1. Iniciar os Serviços

```bash
# Terminal 1 - User Service
UserServiceApp.exe

# Terminal 2 - Order Service
OrderServiceApp.exe

# Terminal 3 - Notification Service
NotificationServiceApp.exe

# Terminal 4 - API Gateway
MicroservicesGateway.exe
```

### 2. Testar via Gateway

```bash
# Listar usuários
curl http://localhost:8080/api/users

# Criar usuário
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{"name":"Carlos Silva","email":"carlos@email.com"}' \
  http://localhost:8080/api/users

# Buscar usuário específico
curl http://localhost:8080/api/users/1

# Criar pedido
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{"user_id":1,"product_name":"Notebook","quantity":1,"price":2500.00}' \
  http://localhost:8080/api/orders

# Enviar notificação
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{"user_id":1,"title":"Pedido Confirmado","message":"Seu pedido foi confirmado!","type":"order"}' \
  http://localhost:8080/api/notifications
```

### 3. Monitoramento

```bash
# Status do gateway
curl http://localhost:8080/health

# Status dos serviços
curl http://localhost:8080/services/status
```

**Resposta do Health Check:**
```json
{
  "status": "healthy",
  "timestamp": "2024-01-15 10:30:45",
  "services": {
    "user-service": true,
    "order-service": true,
    "notification-service": true
  }
}
```

**Resposta do Service Status:**
```json
{
  "gateway": "API Gateway",
  "version": "1.0.0",
  "timestamp": "2024-01-15 10:30:45",
  "services": [
    {
      "name": "user-service",
      "host": "localhost",
      "port": 8081,
      "provider": "indy",
      "available": true,
      "last_check": "2024-01-15 10:30:40"
    },
    {
      "name": "order-service",
      "host": "localhost",
      "port": 8082,
      "provider": "synapse",
      "available": true,
      "last_check": "2024-01-15 10:30:40"
    },
    {
      "name": "notification-service",
      "host": "localhost",
      "port": 8083,
      "provider": "indy",
      "available": true,
      "last_check": "2024-01-15 10:30:40"
    }
  ]
}
```

## 🔧 Recursos Avançados

### 1. Load Balancing

```pascal
// Implementar múltiplas instâncias do mesmo serviço
type
  TServiceInstance = record
    Host: string;
    Port: Integer;
    Available: Boolean;
    Load: Integer;
  end;

  TLoadBalancer = class
  private
    FInstances: TDictionary<string, TArray<TServiceInstance>>;
    FStrategy: string; // 'round_robin', 'least_connections', 'random'
  public
    function GetNextInstance(const ServiceName: string): TServiceInstance;
    procedure UpdateInstanceLoad(const ServiceName, Host: string; Port, Load: Integer);
  end;
```

### 2. Circuit Breaker para RPC

```pascal
// Adicionar circuit breaker nas chamadas RPC
type
  TRPCCircuitBreaker = class
  private
    FFailureCount: Integer;
    FLastFailure: TDateTime;
    FState: string; // 'closed', 'open', 'half_open'
  public
    function CanExecute: Boolean;
    procedure RecordSuccess;
    procedure RecordFailure;
  end;
```

### 3. Service Discovery

```pascal
// Registro automático de serviços
type
  TServiceRegistry = class
  private
    FServices: TDictionary<string, TArray<TServiceInstance>>;
  public
    procedure RegisterService(const Name, Host: string; Port: Integer);
    procedure UnregisterService(const Name, Host: string; Port: Integer);
    function DiscoverServices(const Name: string): TArray<TServiceInstance>;
  end;
```

## 🚀 Próximos Passos

1. **Implementar Service Discovery**: Registro automático de serviços
2. **Adicionar Load Balancing**: Distribuição inteligente de carga
3. **Configurar Distributed Tracing**: Rastreamento de requisições
4. **Implementar Event Sourcing**: Histórico de eventos
5. **Adicionar Message Queue**: Comunicação assíncrona
6. **Configurar API Versioning**: Versionamento de APIs
7. **Implementar Rate Limiting**: Controle de taxa por serviço

## 📚 Recursos Relacionados

- [Exemplo Básico](./basic-server.md)
- [Exemplo com Resiliência](./resilience.md)
- [Exemplo com Plugins](./plugins.md)
- [Guia de Arquitetura](../architecture.md)

---

Este exemplo demonstra como o Nest4D facilita a criação de arquiteturas de microserviços robustas e escaláveis, com comunicação RPC eficiente e monitoramento integrado.