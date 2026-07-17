program ValidationPipeConcurrencyRepro;

{
  ------------------------------------------------------------------------------
  Deterministic thread-safety repro for Nidus's TValidationPipe.

  Background (backend PR#307 follow-up): TValidationPipe is registered as a
  process-wide SINGLETON (GetNidus.UsePipes(TValidationPipe.Create)), but
  Validate kept its per-request working state (FValidations/FTransforms/
  FJsonMapped) in INSTANCE fields, creating them at the top and freeing them in
  the finally on EVERY call. Under concurrent requests (Horse worker threads):
    - thread B overwrites the field with a new dictionary while thread A still
      holds/iterates the old one;
    - both threads then Free the field -> cross-thread double-free / use-after-
      free of TObjectDictionary/TList.
  FastMM caught exactly this in the backend: "virtual method on a freed object"
  and "FreeMem block header corrupted", stack at Validate (the finally Frees) ->
  TDictionary.Destroy -> FreeMem, reached from TNidus.LoadRouteModule per request.
  The backend's route-handlers carry no validation decorators, so the racing
  dictionaries are EMPTY: this harness reproduces the exact production path with a
  bare published controller.

  This console harness makes the race deterministic: ONE shared pipe, N threads
  released together, each hammering Validate/IsMessages/BuildMessages. The default
  memory manager (FastMM) raises EInvalidPointer on the double-free and AV on the
  use-after-free, both catchable here.

  Expected:
    - UNPATCHED source (instance-field state): FAILS (exit 1), usually in round 1.
    - PATCHED source   (per-request locals + per-thread messages): PASSES (exit 0)
      across all rounds.
  ------------------------------------------------------------------------------
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Nidus.Validation.Interfaces in '..\Source\Interfaces\Nidus.Validation.Interfaces.pas',
  Nidus.Request in '..\Source\Core\Nidus.Request.pas',
  Nidus.Validation.Pipe in '..\Source\Pipes\Validators\Nidus.Validation.Pipe.pas';

const
  THREADS         = 8;
  ROUNDS          = 50;
  CALLS_PER_ROUND = 3000;

type
  // The backend controllers carry no validation decorators, so the production race
  // is the unconditional create+free of the (empty) working dictionaries. A bare
  // published method makes _ResolvePayLoads reflect over the class like production.
  {$M+}
  TDummyController = class
  published
    procedure Handle;
  end;
  {$M-}

procedure TDummyController.Handle;
begin
end;

var
  GPipe: IValidationPipe;
  GRequest: IRouteRequest;
  GFailures: Integer = 0;
  GFirstError: string = '';
  GErrLock: TCriticalSection;

procedure RecordError(const AMsg: string);
begin
  TInterlocked.Increment(GFailures);
  GErrLock.Enter;
  try
    if GFirstError = '' then
      GFirstError := AMsg;
  finally
    GErrLock.Leave;
  end;
end;

function MakeRequest: IRouteRequest;
var
  LH, LP, LQ: TStringList;
begin
  LH := TStringList.Create;
  LP := TStringList.Create;
  LQ := TStringList.Create;
  try
    // TRouteRequest copies (Assign) the string lists, so the temporaries are freed.
    Result := TRouteRequest.Create(LH, LP, LQ, '', 'localhost',
      'application/json', 'GET', '/', 0, '');
  finally
    LH.Free;
    LP.Free;
    LQ.Free;
  end;
end;

type
  TResolveThread = class(TThread)
  private
    FStartGate: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AStartGate: TEvent);
  end;

constructor TResolveThread.Create(AStartGate: TEvent);
begin
  FStartGate := AStartGate;
  inherited Create(False);
end;

procedure TResolveThread.Execute;
var
  I: Integer;
begin
  // All threads block here, then are released together to maximize contention on
  // the shared pipe's working state.
  FStartGate.WaitFor(INFINITE);
  for I := 0 to CALLS_PER_ROUND - 1 do
  begin
    try
      GPipe.Validate(TDummyController, GRequest);
      if GPipe.IsMessages then
        GPipe.BuildMessages;
    except
      on E: Exception do
        RecordError(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

procedure RunRound;
var
  LGate: TEvent;
  LThreads: array[0..THREADS - 1] of TResolveThread;
  I: Integer;
begin
  LGate := TEvent.Create(nil, True, False, '');
  try
    for I := 0 to THREADS - 1 do
      LThreads[I] := TResolveThread.Create(LGate);
    LGate.SetEvent;   // release all threads at once
    for I := 0 to THREADS - 1 do
    begin
      LThreads[I].WaitFor;
      LThreads[I].Free;
    end;
  finally
    LGate.Free;
  end;
end;

var
  R: Integer;
begin
  GErrLock := TCriticalSection.Create;
  GPipe := TValidationPipe.Create;
  // One shared request: the bare controller has no decorators, so Validate never
  // reads it — exactly the production path — and it is not the state under test.
  GRequest := MakeRequest;
  try
    Writeln(Format('ValidationPipe concurrency repro: %d threads x %d rounds x %d calls',
      [THREADS, ROUNDS, CALLS_PER_ROUND]));
    for R := 1 to ROUNDS do
    begin
      RunRound;
      if GFailures > 0 then
      begin
        Writeln(Format('  round %d: FAILURES so far = %d', [R, GFailures]));
        Break;
      end;
    end;

    Writeln;
    if GFailures > 0 then
    begin
      Writeln('RESULT: FAIL  (failures=', GFailures, ')');
      Writeln('First error: ', GFirstError);
      ExitCode := 1;
    end
    else
    begin
      Writeln('RESULT: PASS  (0 failures across all rounds)');
      ExitCode := 0;
    end;
  finally
    GRequest := nil;
    GPipe := nil;
    GErrLock.Free;
  end;
end.
