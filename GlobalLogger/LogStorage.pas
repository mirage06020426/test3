unit LogStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils, LoggerBase, McJSON;

type
  { TLogQueueItem - 로그 큐 아이템 }
  TLogQueueItem = record
    Level: TLogLevel;       // 로그 레벨
    Message: string;        // 메시지
    Tag: string;            // 태그
    Timestamp: TDateTime;   // 시간
    JsonData: string;       // 확장 JSON 데이터
  end;
  PLogQueueItem = ^TLogQueueItem;

  { TLogStorageBase - 로그 저장소 기본 클래스 }
  TLogStorageBase = class(TInterfacedObject, ILogStorage)
  private
    FLock: TCriticalSection;         // 스레드 동기화 객체
    FQueue: TThreadList;             // 로그 메시지 큐
    FMaxQueueSize: Integer;          // 큐 최대 크기
    FFlushInterval: Integer;         // 큐 자동 플러시 간격 (ms)
    FLastFlushTime: TDateTime;       // 마지막 큐 플러시 시간
    FUseExtendedData: Boolean;       // 확장 데이터 사용 여부
    
  protected
    { 로그 저장 구현 (자식 클래스에서 오버라이드) }
    procedure DoStoreLog(const LogItem: TLogItem); virtual; abstract;
    procedure DoStoreLogWithExtData(const LogItem: TLogItem; const JsonData: string); virtual; abstract;
    
    { 큐 처리 }
    procedure ProcessQueue; virtual;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    { ILogStorage 인터페이스 구현 }
    procedure StoreLog(const LogItem: TLogItem);
    procedure StoreLogWithExtData(const LogItem: TLogItem; const JsonData: string);
    procedure Flush; virtual;
    procedure Init; virtual;
    procedure Shutdown; virtual;

    { 속성 }
    property MaxQueueSize: Integer read FMaxQueueSize write FMaxQueueSize;
    property FlushInterval: Integer read FFlushInterval write FFlushInterval;
    property UseExtendedData: Boolean read FUseExtendedData write FUseExtendedData;
  end;
  
  { TFileLogStorage - 파일 기반 로그 저장소 }
  TFileLogStorage = class(TLogStorageBase)
  private
    FLogPath: string;             // 로그 파일 경로
    FFilePrefix: string;          // 로그 파일 접두사
    FCurrentFileName: string;     // 현재 로그 파일 이름
    FCurrentDate: TDateTime;      // 현재 로그 날짜
    FJsonLogEnabled: Boolean;     // JSON 로깅 활성화 여부
    FJsonLogPath: string;         // JSON 로그 파일 경로
    
    { 로그 파일 관리 }
    procedure EnsureLogDirectory;
    function GetLogFileName(const ADate: TDateTime): string;
    function GetJsonLogFileName(const ADate: TDateTime): string;
    procedure WriteToFile(const FileName, LogText: string);
    
  protected
    { 로그 저장 구현 }
    procedure DoStoreLog(const LogItem: TLogItem); override;
    procedure DoStoreLogWithExtData(const LogItem: TLogItem; const JsonData: string); override;
    
  public
    constructor Create(const ALogPath, AFilePrefix: string);
    procedure Init; override;
    procedure Shutdown; override;

    { 속성 }
    property LogPath: string read FLogPath write FLogPath;
    property FilePrefix: string read FFilePrefix write FFilePrefix;
    property JsonLogEnabled: Boolean read FJsonLogEnabled write FJsonLogEnabled;
    property JsonLogPath: string read FJsonLogPath write FJsonLogPath;
    property CurrentFileName: string read FCurrentFileName write FCurrentFileName;
  end;

implementation

{ TLogStorageBase }

constructor TLogStorageBase.Create;
begin
  inherited Create;
  
  FLock := TCriticalSection.Create;
  FQueue := TThreadList.Create;
  FMaxQueueSize := 100;
  FFlushInterval := 1000; // 1초
  FLastFlushTime := Now;
  FUseExtendedData := False;
end;

destructor TLogStorageBase.Destroy;
var
  List: TList;
  i: Integer;
  Item: PLogQueueItem;
begin
  // 큐에 남은 항목 처리
  Flush;
  
  // 메모리 해제
  List := FQueue.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      Item := PLogQueueItem(List[i]);
      Dispose(Item);
    end;
    List.Clear;
  finally
    FQueue.UnlockList;
  end;
  
  FQueue.Free;
  FLock.Free;
  
  inherited Destroy;
end;

procedure TLogStorageBase.StoreLog(const LogItem: TLogItem);
var
  QueueItem: PLogQueueItem;
  List: TList;
begin
  // 큐에 로그 항목 추가
  New(QueueItem);
  QueueItem^.Level := LogItem.Level;
  QueueItem^.Message := LogItem.Message;
  QueueItem^.Tag := LogItem.Source;
  QueueItem^.Timestamp := LogItem.TimeStamp;
  QueueItem^.JsonData := '';  // 기본 JSON 데이터는 비어 있음

  // 큐에 추가
  List := FQueue.LockList;
  try
    List.Add(QueueItem);
    
    // 큐 크기 확인 및 필요시 플러시
    if List.Count >= FMaxQueueSize then
      ProcessQueue;
  finally
    FQueue.UnlockList;
  end;
  
  // 자동 플러시 확인 - 마지막 플러시 이후 지정된 시간이 지났으면 큐 플러시
  if MilliSecondsBetween(Now, FLastFlushTime) >= FFlushInterval then
    Flush;
end;

procedure TLogStorageBase.StoreLogWithExtData(const LogItem: TLogItem; const JsonData: string);
var
  QueueItem: PLogQueueItem;
  List: TList;
begin
  // 확장 데이터 사용이 비활성화되어 있으면 일반 로그로 저장
  if not FUseExtendedData then
  begin
    StoreLog(LogItem);
    Exit;
  end;

  // 큐에 로그 항목 추가
  New(QueueItem);
  QueueItem^.Level := LogItem.Level;
  QueueItem^.Message := LogItem.Message;
  QueueItem^.Tag := LogItem.Source;
  QueueItem^.Timestamp := LogItem.TimeStamp;
  QueueItem^.JsonData := JsonData;  // 확장 JSON 데이터 저장

  // 큐에 추가
  List := FQueue.LockList;
  try
    List.Add(QueueItem);
    
    // 큐 크기 확인 및 필요시 플러시
    if List.Count >= FMaxQueueSize then
      ProcessQueue;
  finally
    FQueue.UnlockList;
  end;
  
  // 자동 플러시 확인 - 마지막 플러시 이후 지정된 시간이 지났으면 큐 플러시
  if MilliSecondsBetween(Now, FLastFlushTime) >= FFlushInterval then
    Flush;
end;

procedure TLogStorageBase.ProcessQueue;
var
  List: TList;
  i: Integer;
  Item: PLogQueueItem;
  LogItem: TLogItem;
begin
  List := FQueue.LockList;
  try
    if List.Count = 0 then
      Exit;
    
    // 큐의 항목을 하나씩 처리
    for i := 0 to List.Count - 1 do
    begin
      Item := PLogQueueItem(List[i]);
      
      LogItem := TLogItem.Create(Item^.Level, Item^.Message, Item^.Timestamp, Item^.Tag);
      try
        // 확장 데이터 유무에 따라 저장 메서드 호출
        if Item^.JsonData <> '' then
          DoStoreLogWithExtData(LogItem, Item^.JsonData)
        else
          DoStoreLog(LogItem);
      finally
        LogItem.Free;
      end;
      
      // 메모리 해제
      Dispose(Item);
    end;
    
    // 큐 비우기
    List.Clear;
    
    // 마지막 플러시 시간 업데이트
    FLastFlushTime := Now;
  finally
    FQueue.UnlockList;
  end;
end;

procedure TLogStorageBase.Flush;
begin
  ProcessQueue;
end;

procedure TLogStorageBase.Init;
begin
  // 기본 구현: 아무 작업 없음 (필요시 파생 클래스에서 구현)
end;

procedure TLogStorageBase.Shutdown;
begin
  // 기본 구현: 큐 플러시
  Flush;
end;

{ TFileLogStorage }

constructor TFileLogStorage.Create(const ALogPath, AFilePrefix: string);
begin
  inherited Create;
  
  FLogPath := ALogPath;
  FFilePrefix := AFilePrefix;
  FCurrentDate := Date;
  FCurrentFileName := GetLogFileName(FCurrentDate);
  FJsonLogEnabled := False;
  FJsonLogPath := IncludeTrailingPathDelimiter(ALogPath) + 'json';
  
  EnsureLogDirectory;
end;

procedure TFileLogStorage.Init;
begin
  inherited;

  EnsureLogDirectory; // 로그 디렉토리 생성
end;

procedure TFileLogStorage.Shutdown;
begin
  inherited; // 부모 클래스의 Shutdown 호출 (Flush 수행)
  // 추가 정리 작업이 필요하면 여기에 구현
end;

procedure TFileLogStorage.EnsureLogDirectory;
begin
  // 로그 디렉토리 확인 및 생성
  if not DirectoryExists(FLogPath) then
    ForceDirectories(FLogPath);
    
  // JSON 로그 디렉토리 확인 및 생성
  if FJsonLogEnabled and not DirectoryExists(FJsonLogPath) then
    ForceDirectories(FJsonLogPath);
end;

function TFileLogStorage.GetLogFileName(const ADate: TDateTime): string;
begin
  Result := IncludeTrailingPathDelimiter(FLogPath) + 
            FFilePrefix + FormatDateTime('yyyy-mm-dd', ADate) + '.log';
end;

function TFileLogStorage.GetJsonLogFileName(const ADate: TDateTime): string;
var
  DateDir: string;
begin
  // 날짜별 디렉토리 생성
  DateDir := IncludeTrailingPathDelimiter(FJsonLogPath) + FormatDateTime('yyyy-mm-dd', ADate);
  
  if not DirectoryExists(DateDir) then
    ForceDirectories(DateDir);
    
  Result := IncludeTrailingPathDelimiter(DateDir) + FormatDateTime('yyyy-mm-dd', ADate) + '.jsonl';
end;

procedure TFileLogStorage.WriteToFile(const FileName, LogText: string);
var
  LogFile: TextFile;
begin
  try
    AssignFile(LogFile, FileName);
    
    if FileExists(FileName) then
      Append(LogFile)
    else
      Rewrite(LogFile);
      
    try
      WriteLn(LogFile, LogText);
    finally
      CloseFile(LogFile);
    end;
  except
    on E: Exception do
    begin
      // 파일 쓰기 오류 - 여기서는 간단히 콘솔에 출력
      WriteLn('로그 파일 쓰기 오류: ', E.Message);
    end;
  end;
end;

procedure TFileLogStorage.DoStoreLog(const LogItem: TLogItem);
var
  LogFileName: string;
  FormattedMsg: string;
  LogDate: TDateTime;
begin
  // 날짜가 변경되었는지 확인
  LogDate := DateOf(LogItem.TimeStamp);
  if LogDate <> FCurrentDate then
  begin
    FCurrentDate := LogDate;
    FCurrentFileName := GetLogFileName(FCurrentDate);
  end;
  
  // 로그 메시지 형식화
  FormattedMsg := Format('[%s] [%s] [%s] %s',
                      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', LogItem.TimeStamp),
                       LogLevelToStr(LogItem.Level),
                       LogItem.Source,
                       LogItem.Message]);
  
  // 파일에 쓰기
  WriteToFile(FCurrentFileName, FormattedMsg);
end;

procedure TFileLogStorage.DoStoreLogWithExtData(const LogItem: TLogItem; const JsonData: string);
var
  JsonFileName: string;
  JsonObj: TMcJsonItem;
  MetadataObj: TMcJsonItem;
begin
  // 일반 로그 형식으로 저장
  DoStoreLog(LogItem);

  // JSON 로깅이 활성화되어 있으면 JSON 파일에도 저장
  if FJsonLogEnabled then
  begin
    JsonFileName := GetJsonLogFileName(LogItem.TimeStamp);

    // JSON 객체 생성
    JsonObj := TMcJsonItem.Create;
    try
      // 기본 로그 정보 추가
      JsonObj.Add('timestamp').AsString := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', LogItem.TimeStamp);
      JsonObj.Add('level').AsString := LogLevelToStr(LogItem.Level);
      JsonObj.Add('source').AsString := LogItem.Source;
      JsonObj.Add('message').AsString := LogItem.Message;

      // 확장 데이터가 있으면 추가
      if JsonData <> '' then
      begin
        try
          // JsonData 문자열로부터 새 JSON 객체 생성
          MetadataObj := TMcJsonItem.Create(JsonData);
          try
            // 이 객체를 metadata 속성으로 추가
            JsonObj.Add('metadata').AsObject := MetadataObj;
          finally
            MetadataObj.Free;
          end;
        except
          // 잘못된 JSON 형식이면 문자열로 저장
          JsonObj.Add('metadata').AsString := JsonData;
        end;
      end;

      // JSON Lines 형식으로 파일에 쓰기
      WriteToFile(JsonFileName, JsonObj.AsJSON);
    finally
      JsonObj.Free;
    end;
  end;
end;

end.
