unit LoggerBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  { 로그 레벨 정의 - 모든 컴포넌트에서 공통으로 사용 }
  TLogLevel = (llDevelop, llDebug, llInfo, llWarning, llError, llFatal);
  TLogLevelSet = set of TLogLevel;

  { 로그 레벨 이름 배열 - 문자열 변환용 }
  TLogLevelNames = array[TLogLevel] of string;

const
  { 로그 레벨 이름 상수 }
  LogLevelNames: TLogLevelNames = (
    'DEVELOP', 'DEBUG', 'INFO', 'WARNING', 'ERROR', 'FATAL'
  );

  { 모든 로그 레벨 - 기본값으로 활용 }
  LOG_LEVEL_ALL = [llDevelop, llDebug, llInfo, llWarning, llError, llFatal];

type
  { 공통 소스 정보 구조체 - 로그 소스 식별용 }
  TSourceInfo = record
    HostName: string;           // 호스트 이름
    IPAddress: string;          // IP 주소
    ApplicationName: string;    // 애플리케이션 이름
    ApplicationVersion: string; // 애플리케이션 버전
    UserName: string;           // 사용자 이름
    ProcessID: Cardinal;        // 프로세스 ID
    UniqueInstanceID: string;   // 고유 인스턴스 ID
  end;

  { 소스 식별자 포맷 옵션 }
  TSourceIdentifierFormat = (
    sfHostName,     // 호스트명만
    sfIPAddress,    // IP 주소만
    sfApplication,  // 애플리케이션 정보만
    sfHostAndApp,   // 호스트명 + 애플리케이션
    sfIPAndApp,     // IP + 애플리케이션
    sfFull,         // 모든 정보
    sfCustom        // 사용자 정의
  );

  { 소스 식별자 표시 모드 }
  TSourceDisplayMode = (
    sdmNone,        // 표시 안함
    sdmHandlerOnly, // 핸들러 소스만 표시
    sdmTagOnly,     // 태그만 표시
    sdmBoth,        // 핸들러 소스와 태그 모두 표시
    sdmFull         // 호스트, 앱, 핸들러 소스, 태그 모두 표시
  );

  { 소스 식별자 형식 정보 }
  TSourceFormatInfo = record
    DisplayMode: TSourceDisplayMode; // 표시 모드
    Prefix: string;                  // 전체 접두사
    Suffix: string;                  // 전체 접미사
    Separator: string;               // 구분자
    SourceFirst: Boolean;            // 소스 먼저 표시 여부 (False면 태그 먼저)
    UseColor: Boolean;               // 색상 사용 여부 (콘솔, 창 출력용)
  end;

  { 비동기 처리 모드 }
  TAsyncMode = (
    amNone,    // 동기 처리
    amThread,  // 별도 스레드로 처리
    amQueue    // 큐에 넣고 배치 처리
  );

  { 비동기 처리 설정 }
  TAsyncConfig = record
    Mode: TAsyncMode;       // 비동기 모드
    QueueSize: Integer;     // 큐 크기
    FlushInterval: Integer; // 플러시 간격(ms)
  end;

  { 로그 형식 이벤트 유형 }
  TLogFormatEvent = procedure(const Level: TLogLevel; const Msg: string;
    var FormattedMsg: string) of object;

  { 들여쓰기 관리 모드 }
  TIndentMode = (
    imNone,    // 들여쓰기 없음
    imAuto,    // 함수 진입/종료 자동 감지하여 들여쓰기
    imManual   // 수동 들여쓰기
  );

  { 확장 데이터 처리 모드 }
  TExtDataMode = (
    edmNone,     // 확장 데이터 사용 안함
    edmBasic,    // 기본 확장 데이터만 사용 (호스트명, IP, 앱 이름 등)
    edmFull,     // 모든 확장 데이터 사용 (JSON 포함)
    edmCustom    // 사용자 정의 확장 데이터 필드 사용
  );

  { 기본 로그 아이템 클래스 }
  TLogItem = class
  private
    FLevel: TLogLevel;      // 로그 레벨
    FTimeStamp: TDateTime;  // 시간
    FMessage: string;       // 메시지
    FSource: string;        // 소스 (태그)
  public
    constructor Create; overload;
    constructor Create(ALevel: TLogLevel; const AMessage: string; 
      const ASource: string = ''); overload;
    constructor Create(ALevel: TLogLevel; const AMessage: string; 
      ATimeStamp: TDateTime; const ASource: string = ''); overload;

    property Level: TLogLevel read FLevel write FLevel;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property Message: string read FMessage write FMessage;
    property Source: string read FSource write FSource;
  end;

  { 로그 핸들러 인터페이스 }
  ILogHandler = interface
    ['{84B6EE93-C1D0-4E69-AD35-7F0A5D54D8D3}']
    procedure Deliver(const Msg: string; Level: TLogLevel; const Tag: string);
    procedure SetActive(Value: Boolean);
    procedure SetLogLevels(Value: TLogLevelSet);
    procedure SetAsyncMode(Mode: TAsyncMode);
    procedure SetSourceIdentifier(const Value: string);
    function GetActive: Boolean;
    function GetLogLevels: TLogLevelSet;
    function GetAsyncMode: TAsyncMode;
    function GetSourceIdentifier: string;

    property Active: Boolean read GetActive write SetActive;
    property LogLevels: TLogLevelSet read GetLogLevels write SetLogLevels;
    property AsyncMode: TAsyncMode read GetAsyncMode write SetAsyncMode;
    property SourceIdentifier: string read GetSourceIdentifier write SetSourceIdentifier;
  end;

  { 로그 저장소 인터페이스 }
  ILogStorage = interface
    ['{11B0E1A7-B508-4E17-8237-67E2F0A2C1AF}']
    procedure StoreLog(const LogItem: TLogItem);
    procedure StoreLogWithExtData(const LogItem: TLogItem; const JsonData: string);
    procedure Flush;
    procedure Init;
    procedure Shutdown;
  end;

  { 로그 포맷터 인터페이스 }
  ILogFormatter = interface
    ['{F97C77B8-2CAC-47A3-8359-C75F28723CF9}']
    function FormatLogMessage(const Level: TLogLevel; const Msg: string; 
      const Tag: string = ''): string;
    procedure SetSourceFormat(const Value: TSourceFormatInfo);
    function GetSourceFormat: TSourceFormatInfo;
    procedure SetSourceInfo(const Value: TSourceInfo);
    function GetSourceInfo: TSourceInfo;
    
    property SourceFormat: TSourceFormatInfo read GetSourceFormat write SetSourceFormat;
    property SourceInfo: TSourceInfo read GetSourceInfo write SetSourceInfo;
  end;

  { 로그 설정 인터페이스 }
  ILogConfiguration = interface
    ['{D2AFC2F9-DFBE-4BC7-BAD5-92AF0DDCDBC2}']
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function GetLogLevels: TLogLevelSet;
    function GetIndentMode: TIndentMode;
    function GetSourceFormat: TSourceFormatInfo;
    function GetAsyncConfig: TAsyncConfig;
    function GetExtDataMode: TExtDataMode;
    procedure SetLogLevels(Value: TLogLevelSet);
    procedure SetIndentMode(Value: TIndentMode);
    procedure SetSourceFormat(const Value: TSourceFormatInfo);
    procedure SetAsyncConfig(const Value: TAsyncConfig);
    procedure SetExtDataMode(Value: TExtDataMode);
    
    property LogLevels: TLogLevelSet read GetLogLevels write SetLogLevels;
    property IndentMode: TIndentMode read GetIndentMode write SetIndentMode;
    property SourceFormat: TSourceFormatInfo read GetSourceFormat write SetSourceFormat;
    property AsyncConfig: TAsyncConfig read GetAsyncConfig write SetAsyncConfig;
    property ExtDataMode: TExtDataMode read GetExtDataMode write SetExtDataMode;
  end;

{ 전역 유틸리티 함수 }
procedure DebugToFile(const Msg: string);
{ 로그 레벨을 문자열로 변환 }
function LogLevelToStr(Level: TLogLevel): string;
{ 문자열을 로그 레벨로 변환 }
function StrToLogLevel(const S: string): TLogLevel;
{ 호스트 이름 가져오기 }
function GetHostName: string;
{ IP 주소 가져오기 }
function GetIPAddress: string;
{ 사용자 이름 가져오기 }
function GetUserName: string;
{ 프로세스 ID 가져오기 }
function GetProcessID: Cardinal;
{ 현재 시간을 로그 형식 문자열로 변환 }
function FormatTimestamp(const ADateTime: TDateTime): string;
{ DebugToFile }


implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix, Unix, NetDB,
  {$ENDIF}
  blcksock;

{ TLogItem }

constructor TLogItem.Create;
begin
  inherited Create;
  FTimeStamp := Now;
end;

constructor TLogItem.Create(ALevel: TLogLevel; const AMessage: string; const ASource: string);
begin
  Create;
  FLevel := ALevel;
  FMessage := AMessage;
  FSource := ASource;
end;

constructor TLogItem.Create(ALevel: TLogLevel; const AMessage: string; ATimeStamp: TDateTime; 
  const ASource: string);
begin
  Create(ALevel, AMessage, ASource);
  FTimeStamp := ATimeStamp;
end;

{ 유틸리티 함수 구현 }
procedure DebugToFile(const Msg: string);
var
  F: TextFile;
begin
  AssignFile(F, 'debug.log');
  try
    if FileExists('debug.log') then
      Append(F)
    else
      Rewrite(F);
    WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ': ' + Msg);
  finally
    CloseFile(F);
  end;
end;

function LogLevelToStr(Level: TLogLevel): string;
begin
  Result := LogLevelNames[Level];
end;

function StrToLogLevel(const S: string): TLogLevel;
var
  Level: TLogLevel;
  UpperS: string;
begin
  UpperS := UpperCase(S);
  Result := llInfo; // 기본값

  for Level := Low(TLogLevel) to High(TLogLevel) do
  begin
    if UpperS = LogLevelNames[Level] then
    begin
      Result := Level;
      Break;
    end;
  end;
end;

function GetHostName: string;
var
  Socket: TTCPBlockSocket;
begin
  Result := 'unknown-host';
  
  Socket := TTCPBlockSocket.Create;
  try
    try
      Result := Socket.LocalName;
      if Result = '' then
        Result := 'unknown-host';
    except
      Result := 'unknown-host';
    end;
  finally
    Socket.Free;
  end;
end;

function GetIPAddress: string;
var
  Socket: TTCPBlockSocket;
  HostName: string;
begin
  Result := '127.0.0.1';
  
  Socket := TTCPBlockSocket.Create;
  try
    try
      HostName := Socket.LocalName;
      if HostName <> '' then
      begin
        Result := Socket.ResolveName(HostName);
        if Result = '' then
          Result := '127.0.0.1';
      end;
    except
      Result := '127.0.0.1';
    end;
  finally
    Socket.Free;
  end;
end;

function GetUserName: string;
{$IFDEF MSWINDOWS}
var
  Buffer: array[0..255] of Char;
  Size: DWORD;
{$ENDIF}
{$IFDEF UNIX}
var
  Passwd: PPasswordRecord;
{$ENDIF}
begin
  Result := 'unknown-user';

{$IFDEF MSWINDOWS}
  Size := 256;
  if Windows.GetUserName(Buffer, Size) then
    Result := Buffer
  else
  begin
    FillChar(Buffer, SizeOf(Buffer), 0);
    Size := GetEnvironmentVariable('USERNAME', Buffer, SizeOf(Buffer));
    if Size > 0 then
      Result := Buffer
    else
      Result := 'unknown-user';
  end;

  if Result = '' then
    Result := 'unknown-user';
{$ENDIF}

{$IFDEF UNIX}
  Result := GetEnvironmentVariable('USER');
  
  if Result = '' then
  begin
    Passwd := getpwuid(fpGetUID);
    if Passwd <> nil then
      Result := Passwd^.pw_name
    else
      Result := 'unknown-user';
  end;
{$ENDIF}
end;

function GetProcessID: Cardinal;
begin
{$IFDEF MSWINDOWS}
  Result := GetCurrentProcessId;
{$ENDIF}
{$IFDEF UNIX}
  Result := fpGetPID;
{$ENDIF}
end;

function FormatTimestamp(const ADateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ADateTime);
end;

end.
