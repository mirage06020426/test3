unit GlobalLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils, StdCtrls,
  LoggerBase, BaseHandler, LogFormatter, LogConfig, LogStorage,
  McJSON;

type
  { TGlobalLogger - 메인 로거 클래스 }
  TGlobalLogger = class
  private
    FHandlers: TList;             // 로그 핸들러 목록
    FLock: TCriticalSection;      // 스레드 동기화 객체
    FFormatter: ILogFormatter;    // 로그 포맷터
    FConfig: ILogConfiguration;   // 로그 설정
    FOnFormatLog: TLogFormatEvent; // 로그 형식 이벤트

    // 들여쓰기 관리 관련 필드
    FIndentLevel: Integer;        // 현재 들여쓰기 레벨
    FIndentSize: Integer;         // 들여쓰기 크기
    FIndentChar: Char;            // 들여쓰기 문자

    // 타이밍 측정 관련 필드
    FTimings: TStringList;        // 타이밍 정보 저장용 리스트

    // 확장 데이터 관련 필드
    FUseExtendedData: Boolean;    // 확장 데이터 사용 여부
    FCustomExtDataFields: TStringList; // 사용자 정의 확장 데이터 필드

    // 싱글턴 인스턴스
    class var FInstance: TGlobalLogger;

    // 로그 메시지 포맷팅
    function FormatLogMessage(const Level: TLogLevel; const Msg: string; 
      const Tag: string = ''; Handler: TObject = nil): string;

    // 들여쓰기 문자열 생성
    function GetIndentString: string;

  public
    constructor Create;
    destructor Destroy; override;

    // 싱글턴 인스턴스 접근
    class function GetInstance: TGlobalLogger;
    class procedure FreeInstance;

    // 핸들러 관리
    procedure AddHandler(Handler: TObject);
    procedure RemoveHandler(Handler: TObject; FreeHandler: Boolean = True);
    function FindHandler(HandlerClass: TClass): TObject;
    procedure ClearHandlers;

    // 기본 로깅 메서드
    procedure Log(const Msg: string; Level: TLogLevel = llInfo; const Tag: string = '');
    procedure LogFmt(const Fmt: string; const Args: array of const; 
      Level: TLogLevel = llInfo; const Tag: string = '');
    procedure LogDevelop(const Msg: string; const Tag: string = '');
    procedure LogDebug(const Msg: string; const Tag: string = '');
    procedure LogInfo(const Msg: string; const Tag: string = '');
    procedure LogWarning(const Msg: string; const Tag: string = '');
    procedure LogError(const Msg: string; const Tag: string = '');
    procedure LogFatal(const Msg: string; const Tag: string = '');
    procedure LogException(const Msg: string; E: Exception; 
      Level: TLogLevel = llError; const Tag: string = '');

    // 편의 메서드 - MultiLog 호환
    procedure Send(const Msg: string);
    procedure SendError(const Msg: string);
    procedure SendException(const Msg: string; E: Exception);

    // 핸들러 생성 편의 함수
    function AddFileHandler(const LogPath, FilePrefix: string): TObject;
    function AddMemoHandler(AMemo: TCustomMemo): TObject;
    function AddConsoleHandler: TObject;

    // 초기화 메서드
    procedure Configure(const LogPath, FilePrefix: string;
      AMemo: TCustomMemo = nil;
      ALogLevels: TLogLevelSet = [llDevelop..llFatal]);

    // 세션 시작 메서드
    procedure StartLogSession;

    // 들여쓰기 관리 메서드
    procedure IncreaseIndent;
    procedure DecreaseIndent;
    procedure ResetIndent;
    procedure LogWithIndent(const Msg: string; Level: TLogLevel = llInfo; const Tag: string = '');
    procedure LogFunctionEnter(const FuncName: string);
    procedure LogFunctionExit(const FuncName: string);

    // 타이밍 측정 메서드
    procedure StartTiming(const TimingLabel: string);
    procedure EndTiming(const TimingLabel: string; Level: TLogLevel = llInfo);
    function GetElapsedTime(const TimingLabel: string): Int64;  // 밀리초 단위
    procedure ClearTimings;

    // 필터링 메서드
    procedure EnableFilter(const FilterText: string; CaseSensitive: Boolean = False);
    procedure DisableFilter;

    // 태그 관련 메서드
    procedure LogWithTag(const Tag, Msg: string; Level: TLogLevel = llInfo);
    procedure LogFmtWithTag(const Tag, Fmt: string; const Args: array of const; 
      Level: TLogLevel = llInfo);
    procedure EnableTagFilter(const Tags: array of string);
    procedure DisableTagFilter;

    // 비동기 처리 설정
    procedure EnableAsyncLogging(Enable: Boolean = True);
    procedure ConfigureAsyncLogging(const Config: TAsyncConfig);

    // 소스 식별자 관련 메서드
    procedure EnableSourceIdentifier(Format: TSourceIdentifierFormat = sfHostAndApp; 
      const CustomFormat: string = '');
    procedure SetSourceInfo(const HostName, IPAddress, AppName, AppVersion, UserName: string);
    function GetSourceInfo: TSourceInfo;
    procedure ConfigureSourceFormat(DisplayMode: TSourceDisplayMode = sdmBoth;
      const Prefix: string = '[';
      const Suffix: string = ']';
      const Separator: string = ':';
      SourceFirst: Boolean = True);
    procedure SetHandlerSourceId(Handler: TObject; const SourceId: string);
    procedure AutoConfigureHandlerSources;

    // 확장 데이터 관련 메서드
    procedure SetExtendedDataMode(Mode: TExtDataMode);
    procedure EnableExtendedDataStorage(Enable: Boolean = True);
    procedure AddCustomExtDataField(const FieldName: string);
    procedure RemoveCustomExtDataField(const FieldName: string);
    procedure ClearCustomExtDataFields;
    
    // 설정 파일 관련 메서드
    procedure LoadSettings(const FileName: string);
    procedure SaveSettings(const FileName: string);

    // 속성
    property OnFormatLog: TLogFormatEvent read FOnFormatLog write FOnFormatLog;
    property IndentLevel: Integer read FIndentLevel;
    property IndentSize: Integer read FIndentSize write FIndentSize;
    property IndentChar: Char read FIndentChar write FIndentChar;
    property UseExtendedData: Boolean read FUseExtendedData write FUseExtendedData;
  end;

// 간편한 전역 액세스 함수
function Logger: TGlobalLogger;

implementation

uses
  FileHandler, MemoHandler, ConsoleHandler;

// 타이밍 정보 구조체
type
  TTimingInfo = record
    TimingLabel: string;  // 타이밍 라벨
    StartTime: TDateTime; // 시작 시간
  end;
  PTimingInfo = ^TTimingInfo;

var
  GlobalLoggerInstance: TGlobalLogger = nil;

{ 전역 함수 구현 }

function Logger: TGlobalLogger;
begin
  Result := TGlobalLogger.GetInstance;
end;

{ TGlobalLogger }

constructor TGlobalLogger.Create;
begin
  inherited Create;

  // 기본 초기화
  FLock := TCriticalSection.Create;
  FHandlers := TList.Create;
  
  // 포맷터 및 설정 생성
  FFormatter := LogFormatter.TLogFormatter.Create;
  FConfig := LogConfig.TLogConfiguration.Create;

  // 들여쓰기 초기화
  FIndentLevel := 0;
  FIndentSize := 2;
  FIndentChar := ' ';

  // 타이밍 초기화
  FTimings := TStringList.Create;

  // 확장 데이터 초기화
  FUseExtendedData := False;
  FCustomExtDataFields := TStringList.Create;
end;

destructor TGlobalLogger.Destroy;
begin
  // 핸들러 정리
  ClearHandlers;
  FHandlers.Free;

  // 타이밍 정리
  ClearTimings;
  FTimings.Free;

  // 확장 데이터 필드 정리
  FCustomExtDataFields.Free;

  // 동기화 객체 정리
  FLock.Free;

  inherited;
end;

class function TGlobalLogger.GetInstance: TGlobalLogger;
begin
  if FInstance = nil then
    FInstance := TGlobalLogger.Create;
  Result := FInstance;
end;

class procedure TGlobalLogger.FreeInstance;
begin
  if FInstance <> nil then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

function TGlobalLogger.FormatLogMessage(const Level: TLogLevel; const Msg: string; 
  const Tag: string = ''; Handler: TObject = nil): string;
var
  FormattedMsg: string;
  HandlerId: string;
begin
  // 지정된 핸들러의 소스 ID 가져오기
  HandlerId := '';
  if Handler <> nil then
  begin
    if Handler is TBaseLogHandler then
      HandlerId := TBaseLogHandler(Handler).SourceIdentifier;
  end;

  // 포맷터를 통해 메시지 형식화
  FormattedMsg := FFormatter.FormatLogMessage(Level, Msg, Tag);

  // 들여쓰기 처리
  if FConfig.IndentMode <> imNone then
    FormattedMsg := FormattedMsg + GetIndentString;

  // 사용자 정의 형식이 있는 경우 적용
  if Assigned(FOnFormatLog) then
    FOnFormatLog(Level, Msg, FormattedMsg);

  Result := FormattedMsg;
end;

function TGlobalLogger.GetIndentString: string;
var
  i: Integer;
begin
  if (FConfig.IndentMode = imNone) or (FIndentLevel <= 0) then
    Result := ''
  else
  begin
    SetLength(Result, FIndentLevel * FIndentSize);
    for i := 1 to Length(Result) do
      Result[i] := FIndentChar;
  end;
end;

procedure TGlobalLogger.AddHandler(Handler: TObject);
begin
  if Handler = nil then 
    Exit;

  FLock.Enter;
  try
    if (FHandlers.IndexOf(Handler) < 0) and (Handler is TBaseLogHandler) then
    begin
      FHandlers.Add(Handler);
      TBaseLogHandler(Handler).LogLevels := FConfig.LogLevels; // 기본 로그 레벨 설정
      TBaseLogHandler(Handler).Init; // 핸들러 초기화
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.RemoveHandler(Handler: TObject; FreeHandler: Boolean = True);
var
  Index: Integer;
begin
  if Handler = nil then 
    Exit;

  FLock.Enter;
  try
    Index := FHandlers.IndexOf(Handler);
    if Index >= 0 then
    begin
      if Handler is TBaseLogHandler then
        TBaseLogHandler(Handler).Shutdown; // 핸들러 종료
        
      FHandlers.Delete(Index);
      
      if FreeHandler then
        Handler.Free; // 핸들러 해제
    end;
  finally
    FLock.Leave;
  end;
end;

function TGlobalLogger.FindHandler(HandlerClass: TClass): TObject;
var
  i: Integer;
  Handler: TObject;
begin
  Result := nil;

  FLock.Enter;
  try
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      if Handler is HandlerClass then
      begin
        Result := Handler;
        Break;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.ClearHandlers;
var
  i: Integer;
  Handler: TObject;
begin
  FLock.Enter;
  try
    for i := FHandlers.Count - 1 downto 0 do
    begin
      Handler := TObject(FHandlers[i]);
      if Handler is TBaseLogHandler then
        TBaseLogHandler(Handler).Shutdown;
      Handler.Free;
    end;
    FHandlers.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.Log(const Msg: string; Level: TLogLevel; const Tag: string = '');
var
  i: Integer;
  Handler: TObject;
  FormattedMsg: string;
  JsonData: string;
  JsonObj: TMcJsonItem;
  LogItem: TLogItem;
begin
  if not (Level in FConfig.LogLevels) then 
    Exit;

  // 확장 데이터 준비
  JsonData := '';
  if FUseExtendedData and (FConfig.ExtDataMode <> edmNone) then
  begin
    JsonObj := TMcJsonItem.Create;
    try
      // 기본 정보 추가
      if Tag <> '' then
        JsonObj.Add('tag').AsString:= Tag;

      // 확장 데이터 모드에 따라 필드 추가
      case FConfig.ExtDataMode of
        edmBasic:
          begin
            JsonObj.Add('host_name').AsString:= FFormatter.SourceInfo.HostName;
            JsonObj.Add('ip_address').AsString:= FFormatter.SourceInfo.IPAddress;
            JsonObj.Add('application').AsString:= FFormatter.SourceInfo.ApplicationName
          end;

        edmFull:
          begin
            JsonObj.Add('host_name').AsString:=  FFormatter.SourceInfo.HostName;
            JsonObj.Add('ip_address').AsString:=  FFormatter.SourceInfo.IPAddress;
            JsonObj.Add('application').AsString:=  FFormatter.SourceInfo.ApplicationName;
            JsonObj.Add('app_version').AsString:=  FFormatter.SourceInfo.ApplicationVersion;
            JsonObj.Add('user_name').AsString:=  FFormatter.SourceInfo.UserName;
            JsonObj.Add('process_id').AsInteger:=  FFormatter.SourceInfo.ProcessID;
            JsonObj.Add('instance_id').AsString:=  FFormatter.SourceInfo.UniqueInstanceID;
            JsonObj.Add('timestamp').AsString:=  FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
          end;

        edmCustom:
          begin
            // 사용자 정의 필드 추가
            for i := 0 to FCustomExtDataFields.Count - 1 do
            begin
              if FCustomExtDataFields[i] = 'host_name' then
                JsonObj.Add('host_name').AsString:=  FFormatter.SourceInfo.HostName
              else if FCustomExtDataFields[i] = 'ip_address' then
                JsonObj.Add('ip_address').AsString:=  FFormatter.SourceInfo.IPAddress
              else if FCustomExtDataFields[i] = 'application' then
                JsonObj.Add('application').AsString:=  FFormatter.SourceInfo.ApplicationName
              else if FCustomExtDataFields[i] = 'app_version' then
                JsonObj.Add('app_version').AsString:=  FFormatter.SourceInfo.ApplicationVersion
              else if FCustomExtDataFields[i] = 'user_name' then
                JsonObj.Add('user_name').AsString:=  FFormatter.SourceInfo.UserName
              else if FCustomExtDataFields[i] = 'process_id' then
                JsonObj.Add('process_id').AsInteger:=  FFormatter.SourceInfo.ProcessID
              else if FCustomExtDataFields[i] = 'instance_id' then
                JsonObj.Add('instance_id').AsString:=  FFormatter.SourceInfo.UniqueInstanceID
              else if FCustomExtDataFields[i] = 'timestamp' then
                JsonObj.Add('timestamp').AsString:=  FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
            end;
          end;
      end;

      // JSON 문자열로 변환
      JsonData := JsonObj.AsJson;
    finally
      JsonObj.Free;
    end;
  end;

  FLock.Enter;
  try
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      if (Handler is TBaseLogHandler) and TBaseLogHandler(Handler).Active then
      begin
        // 각 핸들러별 메시지 포맷팅
        FormattedMsg := FormatLogMessage(Level, Msg, Tag, Handler);

        // 확장 데이터 유무에 따라 메시지 전달
        if (JsonData <> '') and (Handler is TLogStorageBase) then
        begin
          // 로그 아이템 생성
          LogItem := TLogItem.Create(Level, Msg, Now, Tag);
          try
            TLogStorageBase(Handler).StoreLogWithExtData(LogItem, JsonData);
          finally
            LogItem.Free;
          end;
        end
        else
        begin
          // 일반 로그 전달
          TBaseLogHandler(Handler).Deliver(FormattedMsg, Level, Tag);
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;

  // 들여쓰기 자동 관리 - 함수 출입 관리
  if FConfig.IndentMode = imAuto then
  begin
    if Level = llDebug then
    begin
      // 함수 진입/종료 감지 - 메시지 내용으로 판단
      if (Pos('>>> 함수 진입:', Msg) > 0) or (Pos('entering', LowerCase(Msg)) > 0) then
        IncreaseIndent
      else if (Pos('<<< 함수 종료:', Msg) > 0) or (Pos('exiting', LowerCase(Msg)) > 0) then
        DecreaseIndent;
    end;
  end;
end;

procedure TGlobalLogger.LogFmt(const Fmt: string; const Args: array of const; 
  Level: TLogLevel = llInfo; const Tag: string = '');
begin
  Log(Format(Fmt, Args), Level, Tag);
end;

procedure TGlobalLogger.LogDevelop(const Msg: string; const Tag: string = '');
begin
  Log(Msg, llDevelop, Tag);
end;

procedure TGlobalLogger.LogDebug(const Msg: string; const Tag: string = '');
begin
  Log(Msg, llDebug, Tag);
end;

procedure TGlobalLogger.LogInfo(const Msg: string; const Tag: string = '');
begin
  Log(Msg, llInfo, Tag);
end;

procedure TGlobalLogger.LogWarning(const Msg: string; const Tag: string = '');
begin
  Log(Msg, llWarning, Tag);
end;

procedure TGlobalLogger.LogError(const Msg: string; const Tag: string = '');
begin
  Log(Msg, llError, Tag);
end;

procedure TGlobalLogger.LogFatal(const Msg: string; const Tag: string = '');
begin
  Log(Msg, llFatal, Tag);
end;

procedure TGlobalLogger.LogException(const Msg: string; E: Exception; 
  Level: TLogLevel = llError; const Tag: string = '');
begin
  Log(Format('%s: %s - %s', [Msg, E.ClassName, E.Message]), Level, Tag);
end;

procedure TGlobalLogger.Send(const Msg: string);
begin
  LogInfo(Msg);
end;

procedure TGlobalLogger.SendError(const Msg: string);
begin
  LogError(Msg);
end;

procedure TGlobalLogger.SendException(const Msg: string; E: Exception);
begin
  LogException(Msg, E);
end;

function TGlobalLogger.AddFileHandler(const LogPath, FilePrefix: string): TObject;
var
  FileStorage: TFileLogStorage;
  FileHandler: TBaseLogHandler;
begin
  // 파일 로그 저장소 생성
  FileStorage := TFileLogStorage.Create(LogPath, FilePrefix);
  
  // 기본 핸들러 생성 및 저장소 연결
  FileHandler := TFileHandler.Create(FileStorage);
  
  // 핸들러 등록
  AddHandler(FileHandler);
  
  Result := FileHandler;
end;

function TGlobalLogger.AddMemoHandler(AMemo: TCustomMemo): TObject;
var
  Handler: TBaseLogHandler;
begin
  // 메모 핸들러 생성
  //Handler := MemoHandler.TMemoHandler.Create(AMemo);
  
  // 핸들러 등록
  AddHandler(Handler);
  
  Result := Handler;
end;

function TGlobalLogger.AddConsoleHandler: TObject;
var
  Handler: TBaseLogHandler;
begin
  // 콘솔 핸들러 생성
  //Handler := ConsoleHandler.TConsoleHandler.Create;
  
  // 핸들러 등록
  AddHandler(Handler);
  
  Result := Handler;
end;

procedure TGlobalLogger.Configure(const LogPath, FilePrefix: string;
  AMemo: TCustomMemo = nil; ALogLevels: TLogLevelSet = [llDevelop..llFatal]);
var
  i: Integer;
  Handler: TObject;
begin
  FLock.Enter;
  try
    // 기존 핸들러 제거
    ClearHandlers;
    
    // 로그 레벨 설정
    FConfig.LogLevels := ALogLevels;
    
    // 파일 핸들러 추가
    try
      AddFileHandler(LogPath, FilePrefix);
    except
      on E: Exception do
        DebugToFile('파일 핸들러 생성 실패: ' + E.Message);
    end;
    
    // 메모 핸들러가 요청된 경우 추가
    if Assigned(AMemo) then
    begin
      try
        AddMemoHandler(AMemo);
      except
        on E: Exception do
          DebugToFile('메모 핸들러 생성 실패: ' + E.Message);
      end;
    end;
    
    // 모든 핸들러에 로그 레벨 설정
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      if Handler is TBaseLogHandler then
        TBaseLogHandler(Handler).LogLevels := ALogLevels;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.StartLogSession;
var
  i: Integer;
  Handler: TObject;
  SessionInfo: string;
begin
  // 세션 시작 메시지 생성
  SessionInfo := Format('===== 로그 세션 시작: %s - %s =====',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
     FFormatter.SourceInfo.ApplicationName]);
     
  // 모든 핸들러에 세션 시작 메시지 전달
  for i := 0 to FHandlers.Count - 1 do
  begin
    Handler := TObject(FHandlers[i]);
    if Handler is TBaseLogHandler then
      TBaseLogHandler(Handler).Deliver(SessionInfo, llInfo, 'SESSION');
  end;
end;

procedure TGlobalLogger.IncreaseIndent;
begin
  FLock.Enter;
  try
    Inc(FIndentLevel);
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.DecreaseIndent;
begin
  FLock.Enter;
  try
    if FIndentLevel > 0 then
      Dec(FIndentLevel);
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.ResetIndent;
begin
  FLock.Enter;
  try
    FIndentLevel := 0;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.LogWithIndent(const Msg: string; Level: TLogLevel; const Tag: string);
begin
  // 인덴트가 이미 적용된 메시지 포맷 사용
  Log(Msg, Level, Tag);
end;

procedure TGlobalLogger.LogFunctionEnter(const FuncName: string);
begin
  Log('>>> 함수 진입: ' + FuncName, llDebug);
  // IncreaseIndent는 자동으로 처리됨 (imAuto 모드인 경우)
end;

procedure TGlobalLogger.LogFunctionExit(const FuncName: string);
begin
  // DecreaseIndent는 자동으로 처리됨 (imAuto 모드인 경우)
  Log('<<< 함수 종료: ' + FuncName, llDebug);
end;

procedure TGlobalLogger.StartTiming(const TimingLabel: string);
var
  Timing: PTimingInfo;
  Index: Integer;
begin
  if TimingLabel = '' then 
    Exit;
    
  FLock.Enter;
  try
    // 이미 존재하는 라벨이면 업데이트
    Index := FTimings.IndexOf(TimingLabel);
    if Index >= 0 then
    begin
      Timing := PTimingInfo(FTimings.Objects[Index]);
      Timing^.StartTime := Now;
    end
    else
    begin
      try
        // 새 타이밍 정보 생성
        New(Timing);
        Timing^.TimingLabel := TimingLabel;
        Timing^.StartTime := Now;
        
        FTimings.AddObject(TimingLabel, TObject(Timing));
      except
        on E: Exception do
        begin
          LogError('타이밍 시작 실패: ' + E.Message);
          if Assigned(Timing) then
            Dispose(Timing);
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.EndTiming(const TimingLabel: string; Level: TLogLevel);
var
  Timing: PTimingInfo;
  Index: Integer;
  ElapsedMS: Int64;
begin
  if TimingLabel = '' then 
    Exit;
    
  ElapsedMS := GetElapsedTime(TimingLabel);
  
  if ElapsedMS >= 0 then
  begin
    // 타이밍 로깅
    Log(Format('타이밍 [%s]: %d ms', [TimingLabel, ElapsedMS]), Level);
    
    // 타이밍 정보 제거
    FLock.Enter;
    try
      Index := FTimings.IndexOf(TimingLabel);
      if Index >= 0 then
      begin
        Timing := PTimingInfo(FTimings.Objects[Index]);
        Dispose(Timing);
        FTimings.Delete(Index);
      end;
    finally
      FLock.Leave;
    end;
  end
  else
    // 타이밍 정보를 찾을 수 없음
    Log(Format('타이밍 [%s]: 시작 정보를 찾을 수 없음', [TimingLabel]), llWarning);
end;

function TGlobalLogger.GetElapsedTime(const TimingLabel: string): Int64;
var
  Timing: PTimingInfo;
  Index: Integer;
begin
  Result := -1;  // 기본값: 타이밍 정보 없음
  
  if TimingLabel = '' then 
    Exit;
    
  FLock.Enter;
  try
    Index := FTimings.IndexOf(TimingLabel);
    if Index >= 0 then
    begin
      Timing := PTimingInfo(FTimings.Objects[Index]);
      Result := MilliSecondsBetween(Now, Timing^.StartTime);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.ClearTimings;
var
  i: Integer;
  Timing: PTimingInfo;
begin
  FLock.Enter;
  try
    for i := 0 to FTimings.Count - 1 do
    begin
      Timing := PTimingInfo(FTimings.Objects[i]);
      Dispose(Timing);
    end;
    FTimings.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.EnableFilter(const FilterText: string; CaseSensitive: Boolean);
var
  i: Integer;
  Handler: TObject;
begin
  if FilterText = '' then 
    Exit;
    
  FLock.Enter;
  try
    // 모든 핸들러에 필터 설정
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      if Handler is TBaseLogHandler then
      begin
        try
          TBaseLogHandler(Handler).EnableFilter(FilterText, CaseSensitive);
        except
          on E: Exception do
            LogError('필터 활성화 실패: ' + E.Message);
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.DisableFilter;
var
  i: Integer;
  Handler: TObject;
begin
  FLock.Enter;
  try
    // 모든 핸들러에 필터 해제
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      if Handler is TBaseLogHandler then
      begin
        try
          TBaseLogHandler(Handler).DisableFilter;
        except
          on E: Exception do
            LogError('필터 비활성화 실패: ' + E.Message);
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.LogWithTag(const Tag, Msg: string; Level: TLogLevel);
begin
  Log(Msg, Level, Tag);
end;

procedure TGlobalLogger.LogFmtWithTag(const Tag, Fmt: string; const Args: array of const; 
  Level: TLogLevel);
begin
  LogWithTag(Tag, Format(Fmt, Args), Level);
end;

procedure TGlobalLogger.EnableTagFilter(const Tags: array of string);
var
  i, j: Integer;
  Handler: TObject;
begin
  FLock.Enter;
  try
    // 모든 핸들러에 태그 필터 설정
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      if Handler is TBaseLogHandler then
      begin
        try
          TBaseLogHandler(Handler).ClearTagFilters;
          for j := Low(Tags) to High(Tags) do
            TBaseLogHandler(Handler).AddTagFilter(Tags[j]);
        except
          on E: Exception do
            LogError('태그 필터 활성화 실패: ' + E.Message);
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.DisableTagFilter;
var
  i: Integer;
  Handler: TObject;
begin
  FLock.Enter;
  try
    // 모든 핸들러에 태그 필터 해제
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      if Handler is TBaseLogHandler then
      begin
        try
          TBaseLogHandler(Handler).ClearTagFilters;
        except
          on E: Exception do
            LogError('태그 필터 비활성화 실패: ' + E.Message);
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.EnableAsyncLogging(Enable: Boolean = True);
var
  Config: TAsyncConfig;
begin
  // 기본 비동기 설정
  Config.Mode := amNone;
  Config.QueueSize := 100;
  Config.FlushInterval := 1000; // 1초
  
  if Enable then
    Config.Mode := amThread;
    
  // 통합 설정 적용
  ConfigureAsyncLogging(Config);
end;

procedure TGlobalLogger.ConfigureAsyncLogging(const Config: TAsyncConfig);
var
  i: Integer;
  Handler: TObject;
begin
  FLock.Enter;
  try
    // 설정 저장
    FConfig.AsyncConfig := Config;

    // 모든 핸들러에 비동기 모드 설정
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      if Handler is ILogHandler then
      begin
        try
          (Handler as ILogHandler).SetAsyncMode(Config.Mode);

          // 큐 크기 및 플러시 간격 설정 (로그 저장소인 경우)
          if Handler is TLogStorageBase then
          begin
            TLogStorageBase(Handler).MaxQueueSize := Config.QueueSize;
            TLogStorageBase(Handler).FlushInterval := Config.FlushInterval;
          end;
        except
          on E: Exception do
            LogError('비동기 로깅 설정 실패: ' + E.Message);
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.EnableSourceIdentifier(Format: TSourceIdentifierFormat = sfHostAndApp; 
  const CustomFormat: string = '');
var
  SourceFormat: TSourceFormatInfo;
begin
  if FFormatter = nil then
    Exit;

  // 포맷터에 소스 식별자 포맷 설정
  TLogFormatter(FFormatter).IdentifierFormat := Format;

  if CustomFormat <> '' then
  begin
    TLogFormatter(FFormatter).CustomFormat := CustomFormat;
    TLogFormatter(FFormatter).IdentifierFormat := sfCustom;
  end;

  // 소스 형식 정보 업데이트
  SourceFormat := FConfig.SourceFormat;
  SourceFormat.DisplayMode := sdmBoth;
  FConfig.SourceFormat := SourceFormat;
end;

procedure TGlobalLogger.SetSourceInfo(const HostName, IPAddress, AppName, AppVersion, UserName: string);
var
  SourceInfo: TSourceInfo;
  Guid: TGUID;
begin
  if FFormatter = nil then
    Exit;
    
  // 소스 정보 구조체 생성
  SourceInfo.HostName := HostName;
  SourceInfo.IPAddress := IPAddress;
  SourceInfo.ApplicationName := AppName;
  SourceInfo.ApplicationVersion := AppVersion;
  SourceInfo.UserName := UserName;
  
  // 프로세스 ID 설정
  SourceInfo.ProcessID := GetProcessID;
  
  // 고유 식별자 생성
  {$IFDEF MSWINDOWS}
  if CreateGUID(Guid) = S_OK then
    SourceInfo.UniqueInstanceID := GUIDToString(Guid) + '-' + IntToStr(SourceInfo.ProcessID)
  else
  {$ENDIF}
  begin
    Randomize;
    SourceInfo.UniqueInstanceID := Format('%s-%d-%d',
      [FormatDateTime('yyyymmddhhnnsszzz', Now),
       SourceInfo.ProcessID,
       Random(1000000)]);
  end;
  
  // 포맷터에 소스 정보 설정
  FFormatter.SourceInfo := SourceInfo;
end;

function TGlobalLogger.GetSourceInfo: TSourceInfo;
begin
  if FFormatter <> nil then
    Result := FFormatter.SourceInfo
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TGlobalLogger.ConfigureSourceFormat(DisplayMode: TSourceDisplayMode = sdmBoth;
  const Prefix: string = '['; const Suffix: string = ']';
  const Separator: string = ':'; SourceFirst: Boolean = True);
var
  SourceFormat: TSourceFormatInfo;
begin
  if FFormatter = nil then
    Exit;
    
  // 소스 형식 설정
  SourceFormat.DisplayMode := DisplayMode;
  SourceFormat.Prefix := Prefix;
  SourceFormat.Suffix := Suffix;
  SourceFormat.Separator := Separator;
  SourceFormat.SourceFirst := SourceFirst;
  SourceFormat.UseColor := False;
  
  // 설정 및 포맷터에 소스 형식 설정
  FConfig.SourceFormat := SourceFormat;
  FFormatter.SourceFormat := SourceFormat;
end;

procedure TGlobalLogger.SetHandlerSourceId(Handler: TObject; const SourceId: string);
begin
  if not Assigned(Handler) then
    Exit;
    
  if Handler is TBaseLogHandler then
  begin
    FLock.Enter;
    try
      TBaseLogHandler(Handler).SourceIdentifier := SourceId;
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TGlobalLogger.AutoConfigureHandlerSources;
var
  i: Integer;
  Handler: TObject;
begin
  FLock.Enter;
  try
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      
      if not (Handler is TBaseLogHandler) then
        Continue;
        
      // 핸들러 타입별 소스 ID 자동 설정
      if Handler is FileHandler.TFileHandler then
        TBaseLogHandler(Handler).SourceIdentifier := 'FILE'
      else if Handler is MemoHandler.TMemoHandler then
        TBaseLogHandler(Handler).SourceIdentifier := 'MEMO'
      else if Handler is ConsoleHandler.TConsoleHandler then
        TBaseLogHandler(Handler).SourceIdentifier := 'CONSOLE'
      else
        TBaseLogHandler(Handler).SourceIdentifier := Handler.ClassName;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.SetExtendedDataMode(Mode: TExtDataMode);
var
  i: Integer;
  Handler: TObject;
begin
  FLock.Enter;
  try
    // 설정 저장
    FConfig.ExtDataMode := Mode;
    
    // 모든 로그 저장소에 확장 데이터 사용 설정
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      if Handler is TLogStorageBase then
        TLogStorageBase(Handler).UseExtendedData := (Mode <> edmNone) and FUseExtendedData;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.EnableExtendedDataStorage(Enable: Boolean = True);
var
  i: Integer;
  Handler: TObject;
begin
  FLock.Enter;
  try
    FUseExtendedData := Enable;
    
    // 모든 로그 저장소에 확장 데이터 사용 설정
    for i := 0 to FHandlers.Count - 1 do
    begin
      Handler := TObject(FHandlers[i]);
      if Handler is TLogStorageBase then
        TLogStorageBase(Handler).UseExtendedData := Enable and (FConfig.ExtDataMode <> edmNone);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.AddCustomExtDataField(const FieldName: string);
begin
  if FieldName = '' then 
    Exit;
    
  FLock.Enter;
  try
    if FCustomExtDataFields.IndexOf(FieldName) < 0 then
      FCustomExtDataFields.Add(FieldName);
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.RemoveCustomExtDataField(const FieldName: string);
var
  Index: Integer;
begin
  if FieldName = '' then 
    Exit;
    
  FLock.Enter;
  try
    Index := FCustomExtDataFields.IndexOf(FieldName);
    if Index >= 0 then
      FCustomExtDataFields.Delete(Index);
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.ClearCustomExtDataFields;
begin
  FLock.Enter;
  try
    FCustomExtDataFields.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TGlobalLogger.LoadSettings(const FileName: string);
begin
  if not Assigned(FConfig) then
    Exit;
    
  try
    // 설정 파일 로드
    FConfig.LoadFromFile(FileName);
    
    // 설정 적용
    FLock.Enter;
    try
      // 소스 형식 설정 포맷터에 전달
      if Assigned(FFormatter) then
        FFormatter.SourceFormat := FConfig.SourceFormat;
        
      // 비동기 설정 적용
      ConfigureAsyncLogging(FConfig.AsyncConfig);
      
      // 확장 데이터 모드 설정
      SetExtendedDataMode(FConfig.ExtDataMode);
    finally
      FLock.Leave;
    end;
    
    LogInfo('설정 파일을 로드했습니다: ' + FileName);
  except
    on E: Exception do
      LogError('설정 파일 로드 실패: ' + E.Message);
  end;
end;

procedure TGlobalLogger.SaveSettings(const FileName: string);
begin
  if not Assigned(FConfig) then
    Exit;
    
  try
    // 설정 파일 저장
    FConfig.SaveToFile(FileName);
    LogInfo('설정 파일을 저장했습니다: ' + FileName);
  except
    on E: Exception do
      LogError('설정 파일 저장 실패: ' + E.Message);
  end;
end;

initialization
  TGlobalLogger.FInstance := nil;

finalization
  TGlobalLogger.FreeInstance;

end.
