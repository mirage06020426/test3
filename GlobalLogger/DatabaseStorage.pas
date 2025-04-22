unit DatabaseStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils, 
  ZConnection, ZDataset, ZDbcIntfs,
  LoggerBase, LogStorage, McJSON;

const
  DEFAULT_RETENTION_MONTHS = 12;

type
  { TLogTableInfo - 월별 로그 테이블 정보 }
  TLogTableInfo = record
    TableName: string;       // 테이블 이름
    YearMonth: string;       // YYYYMM 형식
    CreationDate: TDateTime; // 생성 일자
    LastUpdate: TDateTime;   // 마지막 업데이트
    RecordCount: Integer;    // 대략적인 행 개수
  end;
  
  TLogTableInfoArray = array of TLogTableInfo;

  { TDatabaseLogStorage - 데이터베이스 로그 저장소 }
  TDatabaseLogStorage = class(TLogStorageBase)
  private
    FConnection: TZConnection;         // 데이터베이스 연결
    FLogQuery: TZQuery;                // 로그 쿼리
    FTablePrefix: string;              // 로그 테이블 접두사 (기본값: LOGS)
    FMetaTableName: string;            // 메타 테이블 이름 (기본값: LOG_META)
    FExtDataTableName: string;         // 확장 데이터 테이블 이름 (기본값: LOG_EXTENDED)
    FAutoCreateTable: Boolean;         // 테이블 자동 생성 여부
    FRetentionMonths: Integer;         // 로그 데이터 보관 개월 수
    FLastCleanupDate: TDateTime;       // 마지막 정리 날짜
    FCurrentMonthTable: string;        // 현재 월 테이블 이름
    FLastTableCheck: TDateTime;        // 마지막으로 테이블 체크한 시간
    
    // 연결 정보
    FServerName: string;
    FDatabaseName: string;
    FUserName: string;
    FPassword: string;
    FConnected: Boolean;

    // 비동기 처리 관련 필드
    FLogQueue: TThreadList;

    // JSON 로깅 관련
    FSupportsJsonSearch: Boolean;      // JSON 검색 지원 여부
    
    { 테이블 관리 메서드 }
    function GetMonthlyTableName(const ADate: TDateTime): string;
    function GetCurrentMonthTableName: string;
    procedure CreateMonthlyTable(const ATableName, AYearMonth: string);
    procedure CreateMetaTable;
    procedure CreateExtDataTable;
    procedure UpdateMetaTable(const ATableName, AYearMonth: string; ACreationDate: TDateTime);
    procedure UpdateTableRowCount(const ATableName: string);
    procedure EnsureCurrentMonthTable;
    procedure CleanupOldLogs;
    
    { 데이터베이스 초기화 }
    procedure InitializeTables;
    procedure DetectDatabaseFeatures;
    
    { 쿼리 도우미 }
    function LogLevelToStr(Level: TLogLevel): string;
    function BuildSourceIdentifier(const Tag: string): string;
    function GetLogRecordID: Int64;
    function GetExtDataID: Int64;
    
  protected
    { 로그 저장 구현 }
    procedure DoStoreLog(const LogItem: TLogItem); override;
    procedure DoStoreLogWithExtData(const LogItem: TLogItem; const JsonData: string); override;
    procedure ProcessQueue; override;
    
  public
    constructor Create(const AServerName, ADatabaseName, AUserName, APassword: string);
    destructor Destroy; override;
    
    { 초기화 및 종료 }
    procedure Init; override;
    procedure Shutdown; override;
    procedure Flush; override;
    
    { 데이터베이스 연결 설정 }
    procedure SetConnection(const AServerName, ADatabaseName, AUserName, APassword: string);
    
    { 로그 조회 메서드 }
    function GetLogs(const StartDate, EndDate: TDateTime;
                    const LogLevel: string = '';
                    const SearchText: string = ''): TZQuery;
                    
    function GetLogsWithExtData(const StartDate, EndDate: TDateTime;
                                const LogLevel: string = '';
                                const SearchText: string = ''): TZQuery;
    
    { 데이터베이스 관리 메서드 }
    procedure ExecuteLogMaintenance;
    function GetTablesBetweenDates(StartDate, EndDate: TDateTime): TStringList;
    
    { 확장 데이터 지원 }
    function StoreExtendedData(const SourceInfo: TSourceInfo): Int64;
    function StoreJsonData(const JsonData, DataType: string): Int64;
    function GetLogsByJsonFilter(const JsonPath, JsonValue: string): TZQuery;
    
    { 속성 }
    property TablePrefix: string read FTablePrefix write FTablePrefix;
    property MetaTableName: string read FMetaTableName write FMetaTableName;
    property ExtDataTableName: string read FExtDataTableName write FExtDataTableName;
    property AutoCreateTable: Boolean read FAutoCreateTable write FAutoCreateTable;
    property RetentionMonths: Integer read FRetentionMonths write FRetentionMonths;
    property Connected: Boolean read FConnected;
    property CurrentMonthTable: string read FCurrentMonthTable;
  end;

implementation

{ TDatabaseLogStorage }

constructor TDatabaseLogStorage.Create(const AServerName, ADatabaseName, AUserName, APassword: string);
begin
  inherited Create;
  DebugToFile('Enter TDatabaseLogStorage.Create');

  // 기본값 설정
  FTablePrefix := 'LOGS';
  FMetaTableName := 'LOG_META';
  FExtDataTableName := 'LOG_EXTENDED';
  FAutoCreateTable := True;
  FLastCleanupDate := 0;           // 초기값 0으로 설정해 첫 로그 작성시 정리 실행
  FLastTableCheck := 0;            // 초기화
  FCurrentMonthTable := '';        // 아직 결정되지 않음
  FRetentionMonths := DEFAULT_RETENTION_MONTHS;

  // 데이터베이스 객체 생성
  FConnection := TZConnection.Create(nil);
  FLogQuery := TZQuery.Create(nil);
  FLogQuery.Connection := FConnection;

  // 데이터베이스 기능 플래그 초기화
  FSupportsJsonSearch := False;

  DebugToFile('before: TDatabaseLogStorage.SetConnection');
  // 연결 설정
  SetConnection(AServerName, ADatabaseName, AUserName, APassword);
  DebugToFile('after: TDatabaseLogStorage.SetConnection');
end;

destructor TDatabaseLogStorage.Destroy;
begin
  try
    Shutdown;
    
    // 데이터베이스 객체 해제
    if Assigned(FLogQuery) then
      FreeAndNil(FLogQuery);
    if Assigned(FConnection) then
      FreeAndNil(FConnection);
  except
    on E: Exception do
      DebugToFile('DatabaseStorage 소멸자 오류: ' + E.Message);
  end;
  
  inherited;
end;

procedure TDatabaseLogStorage.Init;
begin
  inherited;
  
  // 데이터베이스 연결이 이미 설정되어 있으면 연결 시도
  if FConnection.Database <> '' then
  begin
    if not FConnection.Connected then
    begin
      try
        FConnection.Connect;
        FConnected := True;
      except
        on E: Exception do
        begin
          DebugToFile('DatabaseStorage 데이터베이스 연결 오류: ' + E.Message);
          Exit; // 연결 실패 시 더 이상 진행하지 않음
        end;
      end;
      
      // 데이터베이스 기능 감지
      DetectDatabaseFeatures;
      
      // 테이블 자동 생성이 활성화되어 있으면 메타 테이블 생성
      if FAutoCreateTable then
      begin
        CreateMetaTable;
        CreateExtDataTable;
        
        // 현재 월에 해당하는 테이블 이름 초기화 및 생성 확인
        FCurrentMonthTable := GetCurrentMonthTableName;
        EnsureCurrentMonthTable;
      end;
      
      // 오래된 로그 정리 실행
      CleanupOldLogs;
    end;
  end;
end;

procedure TDatabaseLogStorage.Shutdown;
begin
  // 큐에 남은 항목 처리
  Flush;
  
  // 데이터베이스 연결 종료
  if FConnection.Connected then
    FConnection.Disconnect;
    
  FConnected := False;
  
  inherited;
end;

procedure TDatabaseLogStorage.SetConnection(const AServerName, ADatabaseName, 
  AUserName, APassword: string);
var
  AppPath: string;
  DbPath: string;
  LogDbFile: string;
begin
  // 이미 연결되어 있으면 먼저 연결 종료
  if Assigned(FConnection) then
    if FConnection.Connected then
        FConnection.Disconnect;
    
  // 애플리케이션 경로와 DB 경로 설정
  AppPath := ExtractFilePath(ParamStr(0));
  DbPath := IncludeTrailingPathDelimiter(AppPath) + 'logs';
  
  // 로그 디렉토리가 없으면 생성
  if not DirectoryExists(DbPath) then
  begin
    try
      ForceDirectories(DbPath);
      DebugToFile('로그 디렉토리 생성: ' + DbPath);
    except
      on E: Exception do
      begin
        DebugToFile('로그 디렉토리 생성 실패: ' + E.Message);
        // 현재 디렉토리로 대체
        DbPath := AppPath;
      end;
    end;
  end;


  // DB 파일 경로 설정 (사용자 지정 이름이 있으면 사용, 없으면 기본 'Log.fdb' 사용)
  if ADatabaseName <> '' then
    LogDbFile := ADatabaseName
  else
    LogDbFile := 'Log.fdb';
    
  LogDbFile := IncludeTrailingPathDelimiter(DbPath) + LogDbFile;

  // 연결 정보 저장
  FServerName := AServerName;
  FDatabaseName := LogDbFile;
  FUserName := AUserName;
  FPassword := APassword;

  DebugToFile('데이터베이스 경로: ' + LogDbFile);
  
  // 연결 정보 설정
  FConnection.Protocol := 'firebird';
  FConnection.ClientCodepage := 'UTF8';
  FConnection.LibraryLocation := AppPath + 'fbclient.dll';
  //FConnection.HostName := AServerName;
  FConnection.Database := LogDbFile;
  FConnection.User := AUserName;
  FConnection.Password := APassword;
  

  // DB가 없으면 생성
  if not FileExists(LogDbFile) then
  begin
    DebugToFile('데이터베이스 파일이 없음, 생성 시도: ' + LogDbFile);
    FConnection.Properties.Clear;
    FConnection.Properties.Values['dialect'] := '3';
    FConnection.Properties.Values['CreateNewDatabase'] :=
      'CREATE DATABASE ' + QuotedStr(LogDbFile) +
      ' USER ' + QuotedStr(AUserName) +
      ' PASSWORD ' + QuotedStr(APassword) +
      ' PAGE_SIZE 16384 DEFAULT CHARACTER SET UTF8';
      
    try
      FConnection.Connect;
      FConnected := FConnection.Connected;
      DebugToFile('데이터베이스 생성 성공');
    except
      on E: Exception do
        DebugToFile('데이터베이스 생성 실패: ' + E.Message);
    end;
  end
  else
  begin
    DebugToFile('기존 데이터베이스 파일에 연결 시도');
    try
      FConnection.Connect;
      FConnected := FConnection.Connected;
      DebugToFile('데이터베이스 연결 성공');
    except
      on E: Exception do
        DebugToFile('데이터베이스 연결 실패: ' + E.Message);
    end;
  end;
  
  // 테이블 생성 시도
  if (FConnected and FAutoCreateTable) then
  begin
    DebugToFile('DetectDatabaseFeatures;');
    // 데이터베이스 기능 감지
    DetectDatabaseFeatures;
    
    DebugToFile('InitializeTables;');
    // 테이블 초기화
    InitializeTables;
    
    DebugToFile('CreateMetaTable;');
    // 메타 테이블 생성
    CreateMetaTable;
    DebugToFile('CreateExtDataTable;');
    CreateExtDataTable;
    
    // 현재 월 테이블 이름 초기화 및 생성 확인
    FCurrentMonthTable := GetCurrentMonthTableName;

    DebugToFile('EnsureCurrentMonthTable;');
    EnsureCurrentMonthTable;
  end;
end;

procedure TDatabaseLogStorage.DetectDatabaseFeatures;
begin
  // Firebird 버전 확인 및 JSON 검색 지원 여부 감지
  try
    // 간단한 JSON 쿼리로 지원 여부 확인
    FLogQuery.Close;
    FLogQuery.SQL.Text := 'SELECT 1 FROM RDB$DATABASE WHERE EXISTS (SELECT 1 FROM RDB$FUNCTIONS WHERE RDB$FUNCTION_NAME = ''JSON_VALUE'')';
    FLogQuery.Open;
    
    FSupportsJsonSearch := not FLogQuery.EOF;
    FLogQuery.Close;
    
    if FSupportsJsonSearch then
      DebugToFile('JSON 검색 기능이 지원됩니다.')
    else
      DebugToFile('JSON 검색 기능이 지원되지 않습니다.');
  except
    // 확인 중 오류 발생 시 지원하지 않는 것으로 간주
    FSupportsJsonSearch := False;
  end;
end;

procedure TDatabaseLogStorage.InitializeTables;
begin
  try
    // LOG_MASTER 테이블 생성
    FLogQuery.SQL.Text :=
      'EXECUTE BLOCK AS ' +
      'BEGIN ' +
      '  IF (NOT EXISTS(SELECT 1 FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ''LOG_MASTER'')) THEN ' +
      '  EXECUTE STATEMENT ' +
      '  ''CREATE TABLE LOG_MASTER ( ' +
      '    LOG_ID INTEGER NOT NULL PRIMARY KEY, ' +
      '    "TIMESTAMP" TIMESTAMP NOT NULL, ' +
      '    LEVEL VARCHAR(10) NOT NULL, ' +
      '    SOURCE VARCHAR(100), ' +
      '    MESSAGE VARCHAR(1000), ' +
      '    RAW_JSON BLOB SUB_TYPE TEXT ' +
      '  )''; ' +
      'END';
    FLogQuery.ExecSQL;
  except
    on E: Exception do
      DebugToFile('LOG_MASTER 테이블 생성: ' + E.Message);
  end;

  try
    // LOG_MASTER의 시퀀스 생성
    FLogQuery.SQL.Text :=
      'EXECUTE BLOCK AS ' +
      'BEGIN ' +
      '  IF (NOT EXISTS(SELECT 1 FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME = ''GEN_LOG_ID'')) THEN ' +
      '  EXECUTE STATEMENT ''CREATE SEQUENCE GEN_LOG_ID''; ' +
      'END';
    FLogQuery.ExecSQL;
  except
    on E: Exception do
      DebugToFile('LOG_MASTER의 시퀀스 생성: ' + E.Message);
  end;

  try
    // 인덱스 생성
    FLogQuery.SQL.Text :=
      'EXECUTE BLOCK AS ' +
      'BEGIN ' +
      '  IF (NOT EXISTS(SELECT 1 FROM RDB$INDICES WHERE RDB$RELATION_NAME = ''LOG_MASTER'' AND RDB$INDEX_NAME = ''IDX_LOG_TIMESTAMP'')) THEN ' +
      '  EXECUTE STATEMENT ''CREATE ASCENDING INDEX IDX_LOG_TIMESTAMP ON LOG_MASTER("TIMESTAMP")''; ' +
      'END';;
    FLogQuery.ExecSQL;
  except
    on E: Exception do
      DebugToFile('LOG_MASTER의 IDX_LOG_TIMESTAMP 인덱스 생성: ' + E.Message);
  end;

  try
    FLogQuery.SQL.Text :=
      'EXECUTE BLOCK AS ' +
      'BEGIN ' +
      '  IF (NOT EXISTS(SELECT 1 FROM RDB$INDICES WHERE RDB$INDEX_NAME = ''IDX_LOG_LEVEL'')) THEN ' +
      '  EXECUTE STATEMENT ''CREATE INDEX IDX_LOG_LEVEL ON LOG_MASTER(LEVEL)''; ' +
      'END';
    FLogQuery.ExecSQL;
  except
    on E: Exception do
      DebugToFile('LOG_MASTER의 IDX_LOG_LEVEL 인덱스 생성: ' + E.Message);
  end;
end;

procedure TDatabaseLogStorage.CreateMetaTable;
begin
  try
    if not FConnection.Connected then
    begin
      try
        FConnection.Connect;
      except
        on E: Exception do
        begin
          DebugToFile('DatabaseStorage 메타 테이블 생성 중 연결 오류: ' + E.Message);
          Exit;
        end;
      end;
    end;
    
    try
      // 메타 테이블 존재 여부 확인
      FLogQuery.Close;
      FLogQuery.SQL.Text :=
        'SELECT COUNT(*) FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ''' + UpperCase(FMetaTableName) + '''';
      FLogQuery.Open;
      
      if FLogQuery.Fields[0].AsInteger = 0 then
      begin
        // 테이블이 없으면 생성
        FLogQuery.Close;
        FLogQuery.SQL.Text :=
          'CREATE TABLE ' + FMetaTableName + ' (' +
          '  TABLE_NAME VARCHAR(63) NOT NULL PRIMARY KEY,' +
          '  YEAR_MONTH VARCHAR(6) NOT NULL,' +
          '  CREATION_DATE DATE NOT NULL,' +
          '  LAST_UPDATE DATE NOT NULL,' +
          '  RECORD_COUNT INTEGER DEFAULT 0' +
          ')';
          
        DebugToFile('메타 테이블 생성 시도: ' + FLogQuery.SQL.Text);
        FLogQuery.ExecSQL;
        DebugToFile('메타 테이블 생성 성공');
        
        // 인덱스 생성
        FLogQuery.SQL.Text :=
          'CREATE INDEX IDX_' + FMetaTableName + '_YM ON ' + FMetaTableName + ' (YEAR_MONTH)';
        FLogQuery.ExecSQL;
        DebugToFile('메타 테이블 인덱스 생성 성공');
      end
      else
      begin
        FLogQuery.Close;
        DebugToFile('메타 테이블이 이미 존재함: ' + FMetaTableName);
      end;
    except
      on E: Exception do
        DebugToFile('DatabaseStorage 메타 테이블 생성 SQL 오류: ' + E.Message);
    end;
  except
    on E: Exception do
      DebugToFile('DatabaseStorage 메타 테이블 생성 오류: ' + E.Message);
  end;
end;

procedure TDatabaseLogStorage.CreateExtDataTable;
begin
  try
    if not FConnection.Connected then
    begin
      try
        FConnection.Connect;
      except
        on E: Exception do
        begin
          DebugToFile('DatabaseStorage 확장 데이터 테이블 생성 중 연결 오류: ' + E.Message);
          Exit;
        end;
      end;
    end;
    
    try
      // 확장 데이터 테이블 존재 여부 확인
      FLogQuery.Close;
      FLogQuery.SQL.Text :=
        'SELECT COUNT(*) FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ''' + 
        UpperCase(FExtDataTableName) + '''';
      FLogQuery.Open;
      
      if FLogQuery.Fields[0].AsInteger = 0 then
      begin
        // 테이블이 없으면 생성
        FLogQuery.Close;
        FLogQuery.SQL.Text :=
          'CREATE TABLE ' + FExtDataTableName + ' (' +
          '  EXT_ID INTEGER NOT NULL PRIMARY KEY,' +
          '  LOG_ID INTEGER,' +
          '  EXT_TYPE VARCHAR(50) NOT NULL,' +
          '  EXT_KEY VARCHAR(100) NOT NULL,' +
          '  EXT_VALUE_STR VARCHAR(1000),' +
          '  EXT_VALUE_BLOB BLOB SUB_TYPE TEXT,' +
          '  CREATE_DATE TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
          ')';
          
        DebugToFile('확장 데이터 테이블 생성 시도: ' + FLogQuery.SQL.Text);
        FLogQuery.ExecSQL;
        DebugToFile('확장 데이터 테이블 생성 성공');
        
        // 시퀀스 생성
        FLogQuery.SQL.Text :=
          'EXECUTE BLOCK AS ' +
          'BEGIN ' +
          '  IF (NOT EXISTS(SELECT 1 FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME = ''GEN_LOG_EXT_ID'')) THEN ' +
          '  EXECUTE STATEMENT ''CREATE SEQUENCE GEN_LOG_EXT_ID''; ' +
          'END';
        FLogQuery.ExecSQL;
        
        // 인덱스 생성
        FLogQuery.SQL.Text :=
          'CREATE INDEX IDX_' + FExtDataTableName + '_LOG_ID ON ' + 
          FExtDataTableName + ' (LOG_ID)';
        FLogQuery.ExecSQL;
        
        FLogQuery.SQL.Text :=
          'CREATE INDEX IDX_' + FExtDataTableName + '_TYPE ON ' + 
          FExtDataTableName + ' (EXT_TYPE)';
        FLogQuery.ExecSQL;
        
        DebugToFile('확장 데이터 테이블 인덱스 생성 성공');
      end
      else
      begin
        FLogQuery.Close;
        DebugToFile('확장 데이터 테이블이 이미 존재함: ' + FExtDataTableName);
      end;
    except
      on E: Exception do
        DebugToFile('DatabaseStorage 확장 데이터 테이블 생성 SQL 오류: ' + E.Message);
    end;
  except
    on E: Exception do
      DebugToFile('DatabaseStorage 확장 데이터 테이블 생성 오류: ' + E.Message);
  end;
end;

function TDatabaseLogStorage.GetMonthlyTableName(const ADate: TDateTime): string;
begin
  Result := Format('%s_%s', [FTablePrefix, FormatDateTime('YYYYMM', ADate)]);
end;

function TDatabaseLogStorage.GetCurrentMonthTableName: string;
begin
  Result := GetMonthlyTableName(Date);
end;

procedure TDatabaseLogStorage.CreateMonthlyTable(const ATableName, AYearMonth: string);
begin
  try
    if not FConnection.Connected then
    begin
      try
        FConnection.Connect;
      except
        on E: Exception do
        begin
          DebugToFile('CreateMonthlyTable::DatabaseStorage 테이블 생성 중 연결 오류: ' + E.Message);
          Exit;
        end;
      end;
    end;
    
    try
      // 테이블 존재 여부 확인
      FLogQuery.Close;
      FLogQuery.SQL.Text :=
        'SELECT COUNT(*) FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ''' + 
        UpperCase(ATableName) + '''';
      FLogQuery.Open;
      
      if FLogQuery.Fields[0].AsInteger = 0 then
      begin
        // 테이블이 없으면 생성 - Firebird 5.0의 TIME WITHOUT TIME ZONE 사용
        FLogQuery.Close;
        FLogQuery.SQL.Text :=
          'CREATE TABLE ' + ATableName + ' (' +
          '  ID INTEGER NOT NULL PRIMARY KEY,' +
          '  LDATE DATE NOT NULL,' +
          '  LTIME TIME WITHOUT TIME ZONE NOT NULL,' + // 밀리초까지 저장 가능한
          '  LLEVEL VARCHAR(20) NOT NULL,' +
          '  LSOURCE VARCHAR(100),' +
          '  LMESSAGE VARCHAR(4000),' +
          '  RAW_JSON BLOB SUB_TYPE TEXT,' +  // 모든 버전에서 사용 가능
          '  EXT_DATA_ID INTEGER' +           // 확장 데이터 ID 추가
          ')';
          
        DebugToFile('Monthly 테이블 생성 시도: ' + FLogQuery.SQL.Text);
        FLogQuery.ExecSQL;
        DebugToFile('Monthly 테이블 생성 성공: ' + ATableName);
        
        // 시퀀스 생성
        FLogQuery.SQL.Text := 'CREATE SEQUENCE SEQ_' + ATableName;
        FLogQuery.ExecSQL;
        DebugToFile('Monthly Table 시퀀스 생성 성공: SEQ_' + ATableName);
        
        // 트리거 생성
        FLogQuery.SQL.Text :=
          'CREATE TRIGGER ' + ATableName + '_BI FOR ' + ATableName + ' ' +
          'ACTIVE BEFORE INSERT POSITION 0 AS ' +
          'BEGIN ' +
          '  IF (NEW.ID IS NULL) THEN ' +
          '    NEW.ID = NEXT VALUE FOR SEQ_' + ATableName + '; ' +
          'END';
        FLogQuery.ExecSQL;
        DebugToFile('Monthly Table 트리거 생성 성공: ' + ATableName + '_BI');
        
        // 인덱스 생성 - 날짜, 레벨, 소스에 대한 인덱스
        FLogQuery.SQL.Text :=
          'CREATE INDEX IDX_' + ATableName + '_DATE ON ' + ATableName + ' (LDATE)';
        FLogQuery.ExecSQL;
        
        FLogQuery.SQL.Text :=
          'CREATE INDEX IDX_' + ATableName + '_LEVEL ON ' + ATableName + ' (LLEVEL)';
        FLogQuery.ExecSQL;
        
        FLogQuery.SQL.Text :=
          'CREATE INDEX IDX_' + ATableName + '_SOURCE ON ' + ATableName + ' (LSOURCE)';
        FLogQuery.ExecSQL;
        
        // 확장 데이터 인덱스
        FLogQuery.SQL.Text :=
          'CREATE INDEX IDX_' + ATableName + '_EXT_DATA ON ' + 
          ATableName + ' (EXT_DATA_ID)';
        FLogQuery.ExecSQL;
        
        DebugToFile('Monthly Table 인덱스 생성 성공: ' + ATableName);
        
        DebugToFile('Meta Table Update: ');
        // 메타 테이블 업데이트
        UpdateMetaTable(ATableName, AYearMonth, Now);
      end
      else
      begin
        FLogQuery.Close;
        DebugToFile('CreateMonthlyTable::테이블이 이미 존재함: ' + ATableName);
        
        DebugToFile('CreateMonthlyTable::메타 테이블에 없으면 추가: ');
        // 메타 테이블에 없으면 추가
        UpdateMetaTable(ATableName, AYearMonth, Now);
      end;
    except
      on E: Exception do
        DebugToFile('CreateMonthlyTable::DatabaseStorage 테이블 생성 SQL 오류: ' + E.Message);
    end;
  except
    on E: Exception do
      DebugToFile('CreateMonthlyTable::DatabaseStorage 테이블 생성 오류: ' + E.Message);
  end;
end;

procedure TDatabaseLogStorage.UpdateMetaTable(const ATableName, AYearMonth: string; 
  ACreationDate: TDateTime);
begin
  try
    // 메타 테이블에 해당 테이블 정보가 있는지 확인
    FLogQuery.Close;
    FLogQuery.SQL.Text :=
      'SELECT COUNT(*) FROM ' + FMetaTableName + ' WHERE TABLE_NAME = :TableName';
    FLogQuery.ParamByName('TableName').AsString := ATableName;
    FLogQuery.Open;
    
    if FLogQuery.Fields[0].AsInteger = 0 then
    begin
      // 없으면 새로 추가
      FLogQuery.Close;
      FLogQuery.SQL.Text :=
        'INSERT INTO ' + FMetaTableName + 
        ' (TABLE_NAME, YEAR_MONTH, CREATION_DATE, LAST_UPDATE, RECORD_COUNT) ' +
        'VALUES (:TableName, :YearMonth, :CreationDate, :LastUpdate, 0)';
      FLogQuery.ParamByName('TableName').AsString := ATableName;
      FLogQuery.ParamByName('YearMonth').AsString := AYearMonth;
      FLogQuery.ParamByName('CreationDate').AsDate := ACreationDate;
      FLogQuery.ParamByName('LastUpdate').AsDate := ACreationDate;
      FLogQuery.ExecSQL;
      
      DebugToFile('메타 테이블에 새 테이블 정보 추가: ' + ATableName);
    end
    else
    begin
      // 있으면 마지막 업데이트 시간만 갱신
      FLogQuery.Close;
      FLogQuery.SQL.Text :=
        'UPDATE ' + FMetaTableName + ' SET LAST_UPDATE = :LastUpdate ' +
        'WHERE TABLE_NAME = :TableName';
      FLogQuery.ParamByName('LastUpdate').AsDate := Now;
      FLogQuery.ParamByName('TableName').AsString := ATableName;
      FLogQuery.ExecSQL;
      
      DebugToFile('메타 테이블 정보 업데이트: ' + ATableName);
    end;
  except
    on E: Exception do
      DebugToFile('메타 테이블 업데이트 오류: ' + E.Message);
  end;
end;

procedure TDatabaseLogStorage.UpdateTableRowCount(const ATableName: string);
var
  RowCount: Integer;
begin
  try
// 테이블 행 수 조회
    FLogQuery.Close;
    FLogQuery.SQL.Text := 'SELECT COUNT(*) FROM ' + ATableName;
    FLogQuery.Open;
    RowCount := FLogQuery.Fields[0].AsInteger;
    FLogQuery.Close;
    
    // 메타 테이블 업데이트
    FLogQuery.SQL.Text :=
      'UPDATE ' + FMetaTableName + ' SET RECORD_COUNT = :RowCount, LAST_UPDATE = :LastUpdate ' +
      'WHERE TABLE_NAME = :TableName';
    FLogQuery.ParamByName('RowCount').AsInteger := RowCount;
    FLogQuery.ParamByName('LastUpdate').AsDate := Now;
    FLogQuery.ParamByName('TableName').AsString := ATableName;
    FLogQuery.ExecSQL;
  except
    on E: Exception do
      DebugToFile('테이블 행 수 업데이트 오류: ' + E.Message);
  end;
end;

procedure TDatabaseLogStorage.EnsureCurrentMonthTable;
var
  CurrentMonth: string;
  CurrentTable: string;
begin
  // 마지막 체크 이후 하루가 지났으면 다시 체크
  if MinutesBetween(Now, FLastTableCheck) < 60 then
    Exit;
    
  try
    // 현재 월에 해당하는 테이블 이름과 월 문자열 가져오기
    CurrentTable := GetCurrentMonthTableName;
    CurrentMonth := FormatDateTime('YYYYMM', Date);

    DebugToFile(Format('CurrentTable: %s, CurrentMonth: %s', [CurrentTable, CurrentMonth]));
    
    // 현재 설정된 테이블이 이번 달 테이블이 아니면 새로 설정
    if FCurrentMonthTable <> CurrentTable then
    begin
      FCurrentMonthTable := CurrentTable;
      DebugToFile('현재 월 테이블 변경: ' + FCurrentMonthTable);
    end;
    
    // 테이블이 없으면 생성
    CreateMonthlyTable(FCurrentMonthTable, CurrentMonth);
    
    // 마지막 체크 시간 업데이트
    FLastTableCheck := Now;
  except
    on E: Exception do
      DebugToFile('현재 월 테이블 확인 오류: ' + E.Message);
  end;
end;

procedure TDatabaseLogStorage.CleanupOldLogs;
var
  CutoffDate: TDateTime;
  YearMonth: Integer;
  CurrentYearMonth: Integer;
  TableName: string;
  YearMonthStr: string;
begin
  try
    // 하루에 한 번만 정리 실행
    if DaysBetween(Now, FLastCleanupDate) < 1 then
      Exit;
      
    if not FConnection.Connected then
      FConnection.Connect;
      
    // 보관 기간이 0 이하인 경우 정리하지 않음
    if FRetentionMonths <= 0 then
      Exit;
      
    // 현재 날짜의 YYYYMM 값 계산
    CurrentYearMonth := StrToInt(FormatDateTime('YYYYMM', Date));
    
    // 보관 기간에 따른 기준 날짜 계산 (오늘로부터 X개월 전)
    CutoffDate := IncMonth(Date, -FRetentionMonths);
    
    // 메타 테이블이 있는지 확인
    FLogQuery.Close;
    FLogQuery.SQL.Text :=
      'SELECT COUNT(*) FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ''' + 
      UpperCase(FMetaTableName) + '''';
    FLogQuery.Open;
    
    if FLogQuery.Fields[0].AsInteger > 0 then
    begin
      // 메타 테이블에서 오래된 테이블 목록 조회
      FLogQuery.Close;
      FLogQuery.SQL.Text :=
        'SELECT TABLE_NAME, YEAR_MONTH FROM ' + FMetaTableName +
        ' ORDER BY YEAR_MONTH';
      FLogQuery.Open;
      
      while not FLogQuery.EOF do
      begin
        YearMonth := StrToIntDef(FLogQuery.FieldByName('YEAR_MONTH').AsString, 0);
        TableName := FLogQuery.FieldByName('TABLE_NAME').AsString;
        
        // 보관 기간보다 오래된 테이블 삭제
        if (YearMonth > 0) and 
           (YearMonth < StrToInt(FormatDateTime('YYYYMM', CutoffDate))) then
        begin
          try
            // 메타 테이블에서 삭제
            FLogQuery.Close;
            FLogQuery.SQL.Text := 'DELETE FROM ' + FMetaTableName + 
                                  ' WHERE TABLE_NAME = :TableName';
            FLogQuery.ParamByName('TableName').AsString := TableName;
            FLogQuery.ExecSQL;
            
            // 시퀀스 삭제
            FLogQuery.Close;
            FLogQuery.SQL.Text := 'DROP SEQUENCE SEQ_' + TableName;
            FLogQuery.ExecSQL;
            
            // 테이블 삭제
            FLogQuery.Close;
            FLogQuery.SQL.Text := 'DROP TABLE ' + TableName;
            FLogQuery.ExecSQL;
            
            DebugToFile(Format('오래된 로그 테이블 삭제: %s (%s)',
                        [TableName, FormatDateTime('YYYYMM', CutoffDate)]));
          except
            on E: Exception do
              DebugToFile('테이블 삭제 오류: ' + TableName + ' - ' + E.Message);
          end;
        end;
        
        FLogQuery.Next;
      end;
    end
    else
    begin
      // 메타 테이블이 없으면 테이블 이름 패턴으로 조회
      FLogQuery.Close;
      FLogQuery.SQL.Text :=
        'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
        'WHERE RDB$RELATION_NAME LIKE ''' + UpperCase(FTablePrefix) + '_%'' ' +
        'AND RDB$SYSTEM_FLAG = 0';
      FLogQuery.Open;
      
      while not FLogQuery.EOF do
      begin
        TableName := Trim(FLogQuery.FieldByName('RDB$RELATION_NAME').AsString);
        
        // 테이블 이름에서 YYYYMM 부분 추출
        if Length(TableName) >= Length(FTablePrefix) + 7 then
        begin
          YearMonthStr := Copy(TableName, Length(FTablePrefix) + 2, 6);
          YearMonth := StrToIntDef(YearMonthStr, 0);
          
          // 보관 기간보다 오래된 테이블 삭제
          if (YearMonth > 0) and 
             (YearMonth < StrToInt(FormatDateTime('YYYYMM', CutoffDate))) then
          begin
            try
              // 시퀀스 삭제
              FLogQuery.Close;
              FLogQuery.SQL.Text := 'DROP SEQUENCE SEQ_' + TableName;
              FLogQuery.ExecSQL;
              
              // 테이블 삭제
              FLogQuery.Close;
              FLogQuery.SQL.Text := 'DROP TABLE ' + TableName;
              FLogQuery.ExecSQL;
              
              DebugToFile(Format('오래된 로그 테이블 삭제: %s (%s)',
                          [TableName, YearMonthStr]));
            except
              on E: Exception do
                DebugToFile(Format('테이블 삭제 오류: %s - %s', [TableName, E.Message]));
            end;
          end;
        end;
        
        FLogQuery.Next;
      end;
    end;
    
    // 마지막 정리 날짜 업데이트
    FLastCleanupDate := Now;
    
    DebugToFile(Format('DatabaseStorage 로그 정리 완료: %d개월 이전 로그 삭제', [FRetentionMonths]));
  except
    on E: Exception do
      DebugToFile('DatabaseStorage 로그 정리 오류: ' + E.Message);
  end;
end;

function TDatabaseLogStorage.GetTablesBetweenDates(StartDate, EndDate: TDateTime): TStringList;
var
  StartYearMonth, EndYearMonth: string;
  TableName: string;
  YearMonth: string;
begin
  Result := TStringList.Create;
  
  try
    // 날짜 범위를 YYYYMM 형식의 문자열로 변환
    StartYearMonth := FormatDateTime('YYYYMM', StartOfTheMonth(StartDate));
    EndYearMonth := FormatDateTime('YYYYMM', StartOfTheMonth(EndDate));
    
    // 메타 테이블이 있는지 확인
    FLogQuery.Close;
    FLogQuery.SQL.Text :=
      'SELECT COUNT(*) FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ''' + 
      UpperCase(FMetaTableName) + '''';
    FLogQuery.Open;
    
    if FLogQuery.Fields[0].AsInteger > 0 then
    begin
      // 메타 테이블에서 조회
      FLogQuery.Close;
      FLogQuery.SQL.Text :=
        'SELECT TABLE_NAME FROM ' + FMetaTableName + ' ' +
        'WHERE YEAR_MONTH >= :StartYM AND YEAR_MONTH <= :EndYM ' +
        'ORDER BY YEAR_MONTH';
      FLogQuery.ParamByName('StartYM').AsString := StartYearMonth;
      FLogQuery.ParamByName('EndYM').AsString := EndYearMonth;
      FLogQuery.Open;
      
      // 결과를 리스트에 추가
      while not FLogQuery.EOF do
      begin
        Result.Add(FLogQuery.FieldByName('TABLE_NAME').AsString);
        FLogQuery.Next;
      end;
    end
    else
    begin
      // 메타 테이블이 없으면 테이블 이름 패턴으로 조회
      FLogQuery.Close;
      FLogQuery.SQL.Text :=
        'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
        'WHERE RDB$RELATION_NAME LIKE ''' + UpperCase(FTablePrefix) + '_%'' ' +
        'AND RDB$SYSTEM_FLAG = 0 ' +
        'ORDER BY RDB$RELATION_NAME';
      FLogQuery.Open;
      
      // 결과에서 날짜 범위에 해당하는 테이블만 필터링
      while not FLogQuery.EOF do
      begin
        TableName := Trim(FLogQuery.FieldByName('RDB$RELATION_NAME').AsString);
        
        // 테이블 이름에서 YYYYMM 부분 추출
        if Length(TableName) >= Length(FTablePrefix) + 7 then
        begin
          YearMonth := Copy(TableName, Length(FTablePrefix) + 2, 6);
          
          // 날짜 범위에 포함되는지 확인
          if (YearMonth >= StartYearMonth) and (YearMonth <= EndYearMonth) then
            Result.Add(TableName);
        end;
        
        FLogQuery.Next;
      end;
    end;
    
    // 결과가 없으면 현재 테이블만 포함
    if Result.Count = 0 then
    begin
      // 현재 달 테이블이라도 확인
      EnsureCurrentMonthTable;
      Result.Add(FCurrentMonthTable);
    end;
  except
    on E: Exception do
    begin
      DebugToFile('날짜 범위 테이블 조회 오류: ' + E.Message);
      Result.Clear; // 오류 시 빈 리스트 반환
    end;
  end;
end;

function TDatabaseLogStorage.GetLogs(const StartDate, EndDate: TDateTime; 
  const LogLevel: string = ''; const SearchText: string = ''): TZQuery;
var
  SQL: TStringList;
  Tables: TStringList;
  i: Integer;
  LevelFilter, SearchFilter: string;
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  Query.Connection := FConnection;
  
  SQL := TStringList.Create;
  Tables := nil;
  
  try
    // 날짜 범위에 해당하는 테이블 목록 가져오기
    Tables := GetTablesBetweenDates(StartDate, EndDate);
    
    // 테이블이 없으면 빈 결과셋 반환
    if Tables.Count = 0 then
    begin
      SQL.Add('SELECT 0 AS ID, CAST(''1900-01-01'' AS DATE) AS LDATE,');
      SQL.Add('CAST(''00:00:00'' AS TIME) AS LTIME,');
      SQL.Add('''NONE'' AS LLEVEL, '''' AS LSOURCE, ''데이터 없음'' AS LMESSAGE,');
      SQL.Add('NULL AS RAW_JSON, NULL AS EXT_DATA_ID');
      SQL.Add('FROM RDB$DATABASE WHERE 1=0');
      
      Query.SQL.Text := SQL.Text;
      Query.Open;
      Result := Query;
      Exit;
    end;
    
    // 필터 준비
    if LogLevel <> '' then
      LevelFilter := Format('LLEVEL = ''%s''', [LogLevel])
    else
      LevelFilter := '';
      
    if SearchText <> '' then
    begin
      SearchFilter := Format('(LSOURCE LIKE ''%%%s%%'' OR LMESSAGE LIKE ''%%%s%%'')',
                         [SearchText, SearchText]);
    end
    else
      SearchFilter := '';
      
    // 여러 테이블을 UNION ALL로 결합
    for i := 0 to Tables.Count - 1 do
    begin
      if i > 0 then
        SQL.Add('UNION ALL');
        
      SQL.Add(Format('SELECT ID, LDATE, LTIME, LLEVEL, LSOURCE, LMESSAGE, ' +
                  'RAW_JSON, EXT_DATA_ID FROM %s', [Tables[i]]));
      SQL.Add('WHERE LDATE >= :StartDate AND LDATE <= :EndDate');
      
      if LevelFilter <> '' then
        SQL.Add('AND ' + LevelFilter);
        
      if SearchFilter <> '' then
        SQL.Add('AND ' + SearchFilter);
    end;
    
    // 최종 정렬
    SQL.Add('ORDER BY LDATE DESC, LTIME DESC');
    
    // 쿼리 실행
    Query.SQL.Text := SQL.Text;
    Query.ParamByName('StartDate').AsDate := StartDate;
    Query.ParamByName('EndDate').AsDate := EndDate;
    Query.Open;
    
    Result := Query;
  finally
    SQL.Free;
    if Assigned(Tables) then
      Tables.Free;
  end;
end;

function TDatabaseLogStorage.GetLogsWithExtData(const StartDate, EndDate: TDateTime;
  const LogLevel: string = ''; const SearchText: string = ''): TZQuery;
var
  SQL: TStringList;
  Tables: TStringList;
  i: Integer;
  LevelFilter, SearchFilter: string;
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  Query.Connection := FConnection;
  
  SQL := TStringList.Create;
  Tables := nil;
  
  try
    // 날짜 범위에 해당하는 테이블 목록 가져오기
    Tables := GetTablesBetweenDates(StartDate, EndDate);
    
    // 테이블이 없으면 빈 결과셋 반환
    if Tables.Count = 0 then
    begin
      SQL.Add('SELECT 0 AS ID, CAST(''1900-01-01'' AS DATE) AS LDATE,');
      SQL.Add('CAST(''00:00:00'' AS TIME) AS LTIME,');
      SQL.Add('''NONE'' AS LLEVEL, '''' AS LSOURCE, ''데이터 없음'' AS LMESSAGE,');
      SQL.Add('NULL AS RAW_JSON, NULL AS EXT_DATA_ID, NULL AS EXT_TYPE,');
      SQL.Add('NULL AS EXT_KEY, NULL AS EXT_VALUE_STR, NULL AS EXT_VALUE_BLOB');
      SQL.Add('FROM RDB$DATABASE WHERE 1=0');
      
      Query.SQL.Text := SQL.Text;
      Query.Open;
      Result := Query;
      Exit;
    end;
    
    // 필터 준비
    if LogLevel <> '' then
      LevelFilter := Format('LLEVEL = ''%s''', [LogLevel])
    else
      LevelFilter := '';
      
    if SearchText <> '' then
    begin
      SearchFilter := Format('(LSOURCE LIKE ''%%%s%%'' OR LMESSAGE LIKE ''%%%s%%'')',
                         [SearchText, SearchText]);
    end
    else
      SearchFilter := '';
      
    // 여러 테이블을 UNION ALL로 결합
    for i := 0 to Tables.Count - 1 do
    begin
      if i > 0 then
        SQL.Add('UNION ALL');
        
      SQL.Add(Format('SELECT l.ID, l.LDATE, l.LTIME, l.LLEVEL, l.LSOURCE, l.LMESSAGE, ' +
                  'l.RAW_JSON, l.EXT_DATA_ID, e.EXT_TYPE, e.EXT_KEY, ' +
                  'e.EXT_VALUE_STR, e.EXT_VALUE_BLOB ' +
                  'FROM %s l ' +
                  'LEFT JOIN %s e ON l.EXT_DATA_ID = e.EXT_ID',
                  [Tables[i], FExtDataTableName]));
      SQL.Add('WHERE l.LDATE >= :StartDate AND l.LDATE <= :EndDate');
      
      if LevelFilter <> '' then
        SQL.Add('AND ' + LevelFilter);
        
      if SearchFilter <> '' then
        SQL.Add('AND ' + SearchFilter);
    end;
    
    // JSON 필드 포함 검색 필터 추가
    if (SearchText <> '') and FSupportsJsonSearch then
    begin
      SQL.Add('OR (e.EXT_VALUE_STR IS NOT NULL AND');
      SQL.Add('    JSON_VALUE(e.EXT_VALUE_STR, ''$.*'') LIKE ''%' + SearchText + '%'')');
    end;
    
    // 최종 정렬
    SQL.Add('ORDER BY LDATE DESC, LTIME DESC');
    
    // 쿼리 실행
    Query.SQL.Text := SQL.Text;
    Query.ParamByName('StartDate').AsDate := StartDate;
    Query.ParamByName('EndDate').AsDate := EndDate;
    Query.Open;
    
    Result := Query;
  finally
    SQL.Free;
    if Assigned(Tables) then
      Tables.Free;
  end;
end;

function TDatabaseLogStorage.GetLogsByJsonFilter(const JsonPath, JsonValue: string): TZQuery;
var
  SQL: string;
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  Query.Connection := FConnection;
  
  if FSupportsJsonSearch then
  begin
    // JSON 검색 지원 시 쿼리
    SQL := 'SELECT l.ID, l.LDATE, l.LTIME, l.LLEVEL, l.LSOURCE, l.LMESSAGE, ' +
           'l.RAW_JSON, l.EXT_DATA_ID ' +
           'FROM ' + FCurrentMonthTable + ' l ' +
           'JOIN ' + FExtDataTableName + ' e ON l.EXT_DATA_ID = e.EXT_ID ' +
           'WHERE JSON_VALUE(e.EXT_VALUE_STR, :JSON_PATH) = :JSON_VALUE ' +
           'ORDER BY l.LDATE DESC, l.LTIME DESC';
           
    Query.SQL.Text := SQL;
    Query.ParamByName('JSON_PATH').AsString := JsonPath;
    Query.ParamByName('JSON_VALUE').AsString := JsonValue;
  end
  else
  begin
    // JSON 검색 미지원 시 텍스트 검색으로 대체
    SQL := 'SELECT l.ID, l.LDATE, l.LTIME, l.LLEVEL, l.LSOURCE, l.LMESSAGE, ' +
           'l.RAW_JSON, l.EXT_DATA_ID ' +
           'FROM ' + FCurrentMonthTable + ' l ' +
           'JOIN ' + FExtDataTableName + ' e ON l.EXT_DATA_ID = e.EXT_ID ' +
           'WHERE e.EXT_VALUE_STR LIKE :SEARCH_PATTERN ' +
           'ORDER BY l.LDATE DESC, l.LTIME DESC';
           
    Query.SQL.Text := SQL;
    Query.ParamByName('SEARCH_PATTERN').AsString := '%' + JsonValue + '%';
  end;
  
  Query.Open;
  Result := Query;
end;

function TDatabaseLogStorage.LogLevelToStr(Level: TLogLevel): string;
begin
  case Level of
    llDevelop: Result := 'DEVELOP';
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO';
    llWarning: Result := 'WARNING';
    llError: Result := 'ERROR';
    llFatal: Result := 'FATAL';
    else Result := 'UNKNOWN';
  end;
end;

function TDatabaseLogStorage.BuildSourceIdentifier(const Tag: string): string;
var
  ProcessID: Cardinal;
  ThreadID: Cardinal;
begin
  // 현재 프로세스 ID와 스레드 ID 가져오기
  ProcessID := GetProcessID;
  ThreadID := GetCurrentThreadID;
  
  // 소스 식별자 형식: TAG-ProcessID-ThreadID
  if Tag <> '' then
    Result := Format('%s-%d-%d', [Tag, ProcessID, ThreadID])
  else
    Result := Format('APP-%d-%d', [ProcessID, ThreadID]);
end;

function TDatabaseLogStorage.GetLogRecordID: Int64;
begin
  Result := 0;
  
  try
    // 새 로그 ID 얻기
    FLogQuery.Close;
    FLogQuery.SQL.Text := 'SELECT NEXT VALUE FOR GEN_LOG_ID FROM RDB$DATABASE';
    FLogQuery.Open;
    Result := FLogQuery.Fields[0].AsInteger;
    FLogQuery.Close;
  except
    on E: Exception do
      DebugToFile('로그 레코드 ID 얻기 오류: ' + E.Message);
  end;
end;

function TDatabaseLogStorage.GetExtDataID: Int64;
begin
  Result := 0;
  
  try
    // 새 확장 데이터 ID 얻기
    FLogQuery.Close;
    FLogQuery.SQL.Text := 'SELECT NEXT VALUE FOR GEN_LOG_EXT_ID FROM RDB$DATABASE';
    FLogQuery.Open;
    Result := FLogQuery.Fields[0].AsInteger;
    FLogQuery.Close;
  except
    on E: Exception do
      DebugToFile('확장 데이터 ID 얻기 오류: ' + E.Message);
  end;
end;

function TDatabaseLogStorage.StoreExtendedData(const SourceInfo: TSourceInfo): Int64;
var
  JsonObj: TMcJsonItem;
  JsonData: string;
begin
  Result := 0;
  
  if not FConnected then
    Exit;
    
  // SourceInfo에서 JSON 데이터 생성
  JsonObj := TMcJsonItem.Create;
  try
    JsonObj.Add('host_name').AsString:= SourceInfo.HostName;
    JsonObj.Add('ip_address').AsString:=  SourceInfo.IPAddress;
    JsonObj.Add('application').AsString:=  SourceInfo.ApplicationName;
    JsonObj.Add('app_version').AsString:=  SourceInfo.ApplicationVersion;
    JsonObj.Add('user_name').AsString:=  SourceInfo.UserName;
    JsonObj.Add('process_id').AsInteger:=  SourceInfo.ProcessID;
    JsonObj.Add('instance_id').AsString:=  SourceInfo.UniqueInstanceID;
    JsonObj.Add('timestamp').AsString:= FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Now);
    
    JsonData := JsonObj.AsJson;
    
    // 데이터베이스에 저장하고 ID 반환
    Result := StoreJsonData(JsonData, 'SOURCE_INFO');
  finally
    JsonObj.Free;
  end;
end;

function TDatabaseLogStorage.StoreJsonData(const JsonData, DataType: string): Int64;
var
  ExtDataID: Int64;
begin
  Result := 0;
  
  if not FConnected then
    Exit;
    
  try
    // 새 확장 데이터 ID 가져오기
    ExtDataID := GetExtDataID;
    if ExtDataID = 0 then
      Exit;
      
    // 확장 데이터 테이블에 저장
    FLogQuery.Close;
    FLogQuery.SQL.Text :=
      'INSERT INTO ' + FExtDataTableName + 
      ' (EXT_ID, LOG_ID, EXT_TYPE, EXT_KEY, EXT_VALUE_STR) ' +
      'VALUES (:EXT_ID, NULL, :EXT_TYPE, :EXT_KEY, :EXT_VALUE_STR)';
    FLogQuery.ParamByName('EXT_ID').AsInteger := ExtDataID;
    FLogQuery.ParamByName('EXT_TYPE').AsString := DataType;
    FLogQuery.ParamByName('EXT_KEY').AsString := 'json_data';
    FLogQuery.ParamByName('EXT_VALUE_STR').AsString := JsonData;
    FLogQuery.ExecSQL;
    
    Result := ExtDataID;
  except
    on E: Exception do
    begin
      DebugToFile('확장 데이터 저장 오류: ' + E.Message);
      Result := 0;
    end;
  end;
end;

procedure TDatabaseLogStorage.ExecuteLogMaintenance;
begin
  // 오래된 로그 정리 실행
  CleanupOldLogs;
  
  // 현재 테이블 행 수 업데이트
  if FCurrentMonthTable <> '' then
    UpdateTableRowCount(FCurrentMonthTable);
end;

procedure TDatabaseLogStorage.DoStoreLog(const LogItem: TLogItem);
var
  LogDate: TDateTime;
  JsonObj: TMcJsonItem;
  RawJson: string;
begin
  if not FConnected then
    Exit;
    
  // 현재 월 테이블 확인 및 필요시 생성
  EnsureCurrentMonthTable;
  
  // 하루에 한 번 오래된 로그 정리
  if DaysBetween(Now, FLastCleanupDate) >= 1 then
    CleanupOldLogs;
    
  try
    // JSON 객체 생성 (RAW_JSON 컬럼용)
    JsonObj := TMcJsonItem.Create;
    try
      JsonObj.Add('timestamp').AsString:= FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', LogItem.TimeStamp);
      JsonObj.Add('level').AsString:= LogLevelToStr(LogItem.Level);
      JsonObj.Add('source').AsString:= LogItem.Source;
      JsonObj.Add('message').AsString:= LogItem.Message;
      
      RawJson := JsonObj.AsJson;
      
      // 로그 레코드 삽입
      FLogQuery.SQL.Text :=
        'INSERT INTO ' + FCurrentMonthTable + 
        ' (LDATE, LTIME, LLEVEL, LSOURCE, LMESSAGE, RAW_JSON, EXT_DATA_ID) ' +
        'VALUES (:LDATE, :LTIME, :LLEVEL, :LSOURCE, :LMESSAGE, :RAW_JSON, NULL)';
      FLogQuery.ParamByName('LDATE').AsDate := DateOf(LogItem.TimeStamp);
      FLogQuery.ParamByName('LTIME').AsTime := TimeOf(LogItem.TimeStamp);
      FLogQuery.ParamByName('LLEVEL').AsString := LogLevelToStr(LogItem.Level);
      FLogQuery.ParamByName('LSOURCE').AsString := LogItem.Source;
      FLogQuery.ParamByName('LMESSAGE').AsString := LogItem.Message;
      FLogQuery.ParamByName('RAW_JSON').AsString := RawJson;
      FLogQuery.ExecSQL;
    finally
      JsonObj.Free;
    end;
    
    // 주기적으로 메타 테이블 업데이트 (한 시간에 한 번)
    if HoursBetween(Now, FLastTableCheck) >= 1 then
    begin
      UpdateTableRowCount(FCurrentMonthTable);
      FLastTableCheck := Now;
    end;
  except
    on E: Exception do
      DebugToFile('DatabaseStorage 로그 저장 오류: ' + E.Message);
  end;
end;

procedure TDatabaseLogStorage.DoStoreLogWithExtData(const LogItem: TLogItem; const JsonData: string);
var
  LogDate: TDateTime;
  ExtDataID: Int64;
  JsonObj: TMcJsonItem;
  RawJson: string;
begin
  if not FConnected then
    Exit;
    
  // 현재 월 테이블 확인 및 필요시 생성
  EnsureCurrentMonthTable;
  
  // 하루에 한 번 오래된 로그 정리
  if DaysBetween(Now, FLastCleanupDate) >= 1 then
    CleanupOldLogs;
    
  try
    // 먼저 확장 데이터 저장
    ExtDataID := StoreJsonData(JsonData, 'EXT_DATA');
    if ExtDataID = 0 then
    begin
      // 확장 데이터 저장 실패 시 일반 로그로 저장
      DoStoreLog(LogItem);
      Exit;
    end;
    
    // JSON 객체 생성 (RAW_JSON 컬럼용)
    JsonObj := TMcJsonItem.Create;
    try
      JsonObj.Add('timestamp').AsString:= FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', LogItem.TimeStamp);
      JsonObj.Add('level').AsString:= LogLevelToStr(LogItem.Level);
      JsonObj.Add('source').AsString:= LogItem.Source;
      JsonObj.Add('message').AsString:= LogItem.Message;
      JsonObj.Add('ext_data_id').AsInteger:= ExtDataID;
      
      RawJson := JsonObj.AsJson;
      
      // 로그 레코드 삽입 (확장 데이터 ID 포함)
      FLogQuery.SQL.Text :=
        'INSERT INTO ' + FCurrentMonthTable + 
        ' (LDATE, LTIME, LLEVEL, LSOURCE, LMESSAGE, RAW_JSON, EXT_DATA_ID) ' +
        'VALUES (:LDATE, :LTIME, :LLEVEL, :LSOURCE, :LMESSAGE, :RAW_JSON, :EXT_DATA_ID)';
      FLogQuery.ParamByName('LDATE').AsDate := DateOf(LogItem.TimeStamp);
      FLogQuery.ParamByName('LTIME').AsTime := TimeOf(LogItem.TimeStamp);
      FLogQuery.ParamByName('LLEVEL').AsString := LogLevelToStr(LogItem.Level);
      FLogQuery.ParamByName('LSOURCE').AsString := LogItem.Source;
      FLogQuery.ParamByName('LMESSAGE').AsString := LogItem.Message;
      FLogQuery.ParamByName('RAW_JSON').AsString := RawJson;
      FLogQuery.ParamByName('EXT_DATA_ID').AsInteger := ExtDataID;
      FLogQuery.ExecSQL;
    finally
      JsonObj.Free;
    end;
    
    // 주기적으로 메타 테이블 업데이트 (한 시간에 한 번)
    if HoursBetween(Now, FLastTableCheck) >= 1 then
    begin
      UpdateTableRowCount(FCurrentMonthTable);
      FLastTableCheck := Now;
    end;
  except
    on E: Exception do
      DebugToFile('DatabaseStorage 확장 데이터 로그 저장 오류: ' + E.Message);
  end;
end;

procedure TDatabaseLogStorage.ProcessQueue;
var
  List: TList;
  i: Integer;
  Item: PLogQueueItem;
  LogItem: TLogItem;
begin
  // 트랜잭션 내에서 처리
  List := FLogQueue.LockList;
  try
    if List.Count = 0 then
      Exit;
      
    try
      if not FConnection.Connected then
        FConnection.Connect;
        
      // 트랜잭션 시작
      if not FConnection.InTransaction then
        FConnection.StartTransaction;
      
      try
        // 각 큐 항목 처리
        for i := 0 to List.Count - 1 do
        begin
          Item := PLogQueueItem(List[i]);
          
          // 로그 아이템 생성
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
        
        // 모든 작업 완료 후 커밋
        if FConnection.InTransaction then
          FConnection.Commit;
          
        // 큐 비우기
        List.Clear;
        
        // 마지막 플러시 시간 업데이트
        //FLastFlushTime := Now;
      except
        on E: Exception do
        begin
          // 오류 발생 시 롤백
          if FConnection.InTransaction then
            FConnection.Rollback;
            
          DebugToFile('DatabaseStorage 큐 처리 오류: ' + E.Message);
        end;
      end;
    except
      on E: Exception do
        DebugToFile('DatabaseStorage 데이터베이스 연결 오류: ' + E.Message);
    end;
  finally
    FLogQueue.UnlockList;
  end;
end;

procedure TDatabaseLogStorage.Flush;
begin
  // 큐 처리
  ProcessQueue;
  
  inherited;
end;

end.
