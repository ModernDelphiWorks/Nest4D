unit Test.Nest4D.Cache;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  Nest4D.cache;

[TestFixture]
TTestNest4DCache = class
private
  FCache: ICacheManager;
public
  [Setup]
  procedure Setup;
  [TearDown]
  procedure TearDown;
  
  [Test]
  procedure TestCacheCreation;
  
  [Test]
  procedure TestCacheSetAndGet;
  
  [Test]
  procedure TestCacheExpiration;
  
  [Test]
  procedure TestCacheDelete;
  
  [Test]
  procedure TestCacheExists;
  
  [Test]
  procedure TestCacheClear;
end;

implementation

procedure TTestNest4DCache.Setup;
begin
  FCache := GetCacheManager;
end;

procedure TTestNest4DCache.TearDown;
begin
  if Assigned(FCache) then
    FCache.Clear;
  FCache := nil;
end;

procedure TTestNest4DCache.TestCacheCreation;
begin
  Assert.IsNotNull(FCache, 'Cache manager should be created successfully');
end;

procedure TTestNest4DCache.TestCacheSetAndGet;
var
  LValue: string;
begin
  // Set a value in cache
  Assert.WillNotRaise(
    procedure
    begin
      FCache.SetValue('test_key', 'test_value');
    end,
    'Setting cache value should not raise exception'
  );
  
  // Get the value back
  Assert.WillNotRaise(
    procedure
    begin
      LValue := FCache.GetValue('test_key');
    end,
    'Getting cache value should not raise exception'
  );
  
  Assert.AreEqual('test_value', LValue, 'Retrieved value should match stored value');
end;

procedure TTestNest4DCache.TestCacheExpiration;
var
  LValue: string;
begin
  // Set a value with short expiration (1 second)
  FCache.SetValue('expire_key', 'expire_value', 1);
  
  // Value should exist immediately
  LValue := FCache.GetValue('expire_key');
  Assert.AreEqual('expire_value', LValue, 'Value should exist before expiration');
  
  // Wait for expiration
  Sleep(1100); // Wait a bit more than 1 second
  
  // Value should be expired
  LValue := FCache.GetValue('expire_key');
  Assert.IsEmpty(LValue, 'Value should be empty after expiration');
end;

procedure TTestNest4DCache.TestCacheDelete;
var
  LValue: string;
begin
  // Set a value
  FCache.SetValue('delete_key', 'delete_value');
  
  // Verify it exists
  LValue := FCache.GetValue('delete_key');
  Assert.AreEqual('delete_value', LValue, 'Value should exist before deletion');
  
  // Delete the value
  Assert.WillNotRaise(
    procedure
    begin
      FCache.DeleteValue('delete_key');
    end,
    'Deleting cache value should not raise exception'
  );
  
  // Verify it's gone
  LValue := FCache.GetValue('delete_key');
  Assert.IsEmpty(LValue, 'Value should be empty after deletion');
end;

procedure TTestNest4DCache.TestCacheExists;
begin
  // Set a value
  FCache.SetValue('exists_key', 'exists_value');
  
  // Check if it exists
  Assert.IsTrue(FCache.Exists('exists_key'), 'Key should exist after setting');
  
  // Check non-existent key
  Assert.IsFalse(FCache.Exists('non_existent_key'), 'Non-existent key should not exist');
end;

procedure TTestNest4DCache.TestCacheClear;
begin
  // Set multiple values
  FCache.SetValue('key1', 'value1');
  FCache.SetValue('key2', 'value2');
  FCache.SetValue('key3', 'value3');
  
  // Verify they exist
  Assert.IsTrue(FCache.Exists('key1'), 'Key1 should exist');
  Assert.IsTrue(FCache.Exists('key2'), 'Key2 should exist');
  Assert.IsTrue(FCache.Exists('key3'), 'Key3 should exist');
  
  // Clear cache
  Assert.WillNotRaise(
    procedure
    begin
      FCache.Clear;
    end,
    'Clearing cache should not raise exception'
  );
  
  // Verify all keys are gone
  Assert.IsFalse(FCache.Exists('key1'), 'Key1 should not exist after clear');
  Assert.IsFalse(FCache.Exists('key2'), 'Key2 should not exist after clear');
  Assert.IsFalse(FCache.Exists('key3'), 'Key3 should not exist after clear');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestNest4DCache);

end.