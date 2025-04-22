unit FileHandler;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, DateUtils, SyncObjs, zipper,
  LoggerBase, BaseHandler, LogStorage;

type
  // 로그 파일 회전 모드
  TLogRotationMode = (lrmDate, lrmSize, lrmBoth);

  // 로그 파일 압축 옵션
  TLogCompressionOptions = record
    Enabled: Boolean;          // 압축 활성화 여부
    AfterDays: Integer;        // 며칠 이상된 파일 압축
    DeleteOriginal: Boolean;   // 압축 후 원본 삭제 여부
  end;

  { TFileHandlerThread - 비동기 파일 로깅용 스레드 }
  TFileHandlerThread = class(TThread)
  private
    FOwner: TObject;          // 소유자 (TFileHandler)
    FQueueEvent: TEvent;      // 큐 이벤트
    FTerminateEvent: TEvent;  // 종료 이벤트
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;

    procedure SignalQueue;    // 큐 처리 신호
    procedure SignalTerminate; // 종료 신호
  end;

  { TFileHandler - 파일 로그 핸들러 }
  TFileHandler = class(TBaseLogHandler)
  private
    FStorage: TFileLogStorage;    // 파일 로그 저장소
    FMaxLogFiles: Integer;        // 최대 로그 파일 수
    FRotationMode: TLogRotationMode; // 로그 파일 회전 모드
    FCompression: TLogCompressionOptions; // 로그 압축 옵션

    // 비동기 처리 관련 필드
    FAsyncThread: TFileHandlerThread; // 비동기 처리 스레드

    procedure CleanupOldLogFiles; // 오래된 로그 파일 정리
    procedure CompressOldLogFiles; // 오래된 로그 파일 압축

  protected
    procedure WriteLog(const Msg: string; Level: TLogLevel); override;

  public
    constructor Create(AStorage: TFileLogStorage); reintroduce; overload;
    constructor Create(const LogPath, FilePrefix: string); reintroduce; overload;
    destructor Destroy; override;

    procedure Init; override;
    procedure Shutdown; override;

    // 세션 시작 마커 기록
    procedure WriteSessionStart;

    // 비동기 모드 설정 오버라이드
   procedure SetAsyncMode(Mode: TAsyncMode); override;
    // 로그 압축 설정
    procedure EnableCompression(Enable: Boolean = True; AfterDays: Integer = 7; DeleteOriginal: Boolean = True);

    // 속성
    property Storage: TFileLogStorage read FStorage;
    property MaxLogFiles: Integer read FMaxLogFiles write FMaxLogFiles;
    property RotationMode: TLogRotationMode read FRotationMode write FRotationMode;
  end;

implementation

{ TFileHandlerThread }

constructor TFileHandlerThread.Create(AOwner: TObject);
begin
  inherited Create(True); // 일시 중단 상태로 생성

  FOwner := AOwner;
  FQueueEvent := TEvent.Create(nil, False, False, '');
  FTerminateEvent := TEvent.Create(nil, True, False, '');

  FreeOnTerminate := False;

  // 스레드 시작
  Start;
end;

destructor TFileHandlerThread.Destroy;
begin
  FQueueEvent.Free;
  FTerminateEvent.Free;

  inherited;
end;

procedure TFileHandlerThread.Execute;
var
  WaitResult: DWORD;
  Events: array[0..1] of THandle;
begin
  Events[0] := THandle(FQueueEvent.Handle);  // 큐 이벤트 핸들
  Events[1] := THandle(FTerminateEvent.Handle);  // 종료 이벤트 핸들

  while not Terminated do
  begin
    // 이벤트 대기 (5초 타임아웃)
    WaitResult := WaitForMultipleObjects(2, @Events[0], False, 5000);

    if Terminated then
      Break;

    case WaitResult of
      WAIT_OBJECT_0:     // FQueueEvent 신호 (인덱스 0)
        TFileHandler(FOwner).Storage.Flush;
      WAIT_OBJECT_0 + 1: // FTerminateEvent 신호 (인덱스 1)
        Break;
      WAIT_TIMEOUT:      // 타임아웃 (5초 경과)
        TFileHandler(FOwner).Storage.Flush;
      else               // 오류 발생 (WAIT_FAILED 등)
        Break;
    end;
  end;
end;

procedure TFileHandlerThread.SignalQueue;
begin
  FQueueEvent.SetEvent;
end;

procedure TFileHandlerThread.SignalTerminate;
begin
  FTerminateEvent.SetEvent;
end;

{ TFileHandler }

constructor TFileHandler.Create(AStorage: TFileLogStorage);
begin
  inherited Create;

  FStorage := AStorage;
  FMaxLogFiles := 10;
  FRotationMode := lrmDate;

  // 압축 옵션
  FCompression.Enabled := False;
  FCompression.AfterDays := 7;
  FCompression.DeleteOriginal := True;

  // 비동기 처리 관련
  FAsyncThread := nil;
end;

constructor TFileHandler.Create(const LogPath, FilePrefix: string);
begin
  FStorage := TFileLogStorage.Create(LogPath, FilePrefix);
  Create(Storage);
end;

destructor TFileHandler.Destroy;
begin
  Shutdown;

  // 저장소 해제
  if Assigned(FStorage) then
    FStorage.Free;

  inherited;
end;

procedure TFileHandler.Init;
begin
  inherited;

  // 저장소 초기화
  if Assigned(FStorage) then
    FStorage.Init;
end;

procedure TFileHandler.Shutdown;
begin
  // 비동기 스레드 종료
  SetAsyncMode(amNone);

  // 저장소 종료
  if Assigned(FStorage) then
    FStorage.Shutdown;

  inherited;
end;

procedure TFileHandler.WriteLog(const Msg: string; Level: TLogLevel);
var
  LogItem: TLogItem;
begin
  if not Assigned(FStorage) then
    Exit;

  // 로그 아이템 생성
  LogItem := TLogItem.Create(Level, Msg);
  try
    // 저장소에 저장
    FStorage.StoreLog(LogItem);
  finally
    LogItem.Free;
  end;
end;

procedure TFileHandler.WriteSessionStart;
var
  AppName: string;
  SessionStartMsg: string;
begin
  AppName := ExtractFileName(ParamStr(0));
  SessionStartMsg := Format('=== Log Session Started at %s by %s ===',
                           [DateTimeToStr(Now), AppName]);

  // 세션 시작 메시지 기록
  WriteLog(SessionStartMsg, llInfo);
end;

procedure TFileHandler.SetAsyncMode(Mode: TAsyncMode);
begin
  // 현재 모드와 같으면 아무것도 하지 않음
  if Mode = AsyncMode then
    Exit;

  inherited SetAsyncMode(Mode);

  // 저장소 비동기 모드 설정
  if Assigned(FStorage) then
  begin
    case Mode of
      amNone:
        begin
          // 비동기 모드 비활성화
          if Assigned(FAsyncThread) then
          begin
            FAsyncThread.SignalTerminate;
            FAsyncThread.Terminate;
            FAsyncThread.WaitFor;
            FAsyncThread.Free;
            FAsyncThread := nil;
          end;

          // 큐에 남은 로그 처리
          FStorage.Flush;
        end;

      amThread:
        begin
          // 비동기 스레드 모드 활성화
          if not Assigned(FAsyncThread) then
            FAsyncThread := TFileHandlerThread.Create(Self);
        end;
    end;
  end;
end;

procedure TFileHandler.EnableCompression(Enable: Boolean; AfterDays: Integer; DeleteOriginal: Boolean);
begin
  FCompression.Enabled := Enable;
  FCompression.AfterDays := AfterDays;
  FCompression.DeleteOriginal := DeleteOriginal;

  // 즉시 압축 실행
  if Enable then
    CompressOldLogFiles;
end;

procedure TFileHandler.CleanupOldLogFiles;
var
  LogFiles: TStringList;
  SearchRec: TSearchRec;
  LogPath: string;
  FilePattern: string;
  i: Integer;
begin
  if FMaxLogFiles <= 0 then Exit;
  if not Assigned(FStorage) then Exit;

  LogPath := FStorage.LogPath;
  FilePattern := FStorage.FilePrefix + '_*.*';

  LogFiles := TStringList.Create;
  try
    // 모든 로그 파일 찾기 (.log와 .zip 모두)
    if FindFirst(LogPath + FilePattern, faAnyFile, SearchRec) = 0 then
    begin
      try
        repeat
          if (LowerCase(ExtractFileExt(SearchRec.Name)) = '.log') or
             (LowerCase(ExtractFileExt(SearchRec.Name)) = '.zip') then
            LogFiles.Add(LogPath + SearchRec.Name);
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;

    // 날짜순으로 정렬 (가장 오래된 파일이 먼저 오도록)
    LogFiles.Sort;

    // 오래된 파일 삭제
    while LogFiles.Count > FMaxLogFiles do
    begin
      try
        DeleteFile(LogFiles[0]);
      except
        // 삭제 실패 처리
      end;
      LogFiles.Delete(0);
    end;
  finally
    LogFiles.Free;
  end;
end;

procedure TFileHandler.CompressOldLogFiles;
var
  SearchRec: TSearchRec;
  LogPath: string;
  FilePattern: string;
  LogFileName, ZipFileName: string;
  FileDate: TDateTime;
  Zipper: TZipper;
  ZipFile: TZipFileEntry;
begin
  if not FCompression.Enabled then Exit;
  if not Assigned(FStorage) then Exit;

  LogPath := FStorage.LogPath;
  FilePattern := FStorage.FilePrefix + '_*.log';

  // 모든 로그 파일 검색
  if FindFirst(LogPath + FilePattern, faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        LogFileName := LogPath + SearchRec.Name;

        // 현재 사용 중인 로그 파일은 건너뜀
        if LogFileName = FStorage.CurrentFileName then
          Continue;

        // 파일 날짜 확인
        FileDate := FileDateToDateTime(SearchRec.Time);

        // 기준일 이전 파일만 압축
        if (Now - FileDate) > FCompression.AfterDays then
        begin
          ZipFileName := ChangeFileExt(LogFileName, '.zip');

          // 이미 압축 파일이 있는지 확인
          if not FileExists(ZipFileName) then
          begin
            try
              Zipper := TZipper.Create;
              try
                Zipper.FileName := ZipFileName;
                ZipFile := Zipper.Entries.AddFileEntry(LogFileName, ExtractFileName(LogFileName));
                Zipper.ZipAllFiles;
              finally
                Zipper.Free;
              end;

              // 압축 성공 시 원본 파일 삭제 (옵션에 따라)
              if FCompression.DeleteOriginal then
                DeleteFile(LogFileName);
            except
              // 압축 실패 처리
            end;
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

end.
