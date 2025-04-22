unit LogFormatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils, LoggerBase;

type
  { TLogFormatter - 로그 메시지 포맷팅 클래스 }
  TLogFormatter = class(TInterfacedObject, ILogFormatter)
  private
    FLock: TCriticalSection;          // 스레드 동기화 객체
    FSourceInfo: TSourceInfo;         // 소스 정보
    FSourceFormat: TSourceFormatInfo; // 소스 식별자 형식 설정
    FIdentifierFormat: TSourceIdentifierFormat; // 소스 식별자 포맷
    FCustomFormat: string;           // 사용자 정의 포맷
    
    // 시간 캐싱 변수
    FLastTimestamp: TDateTime;
    FFormattedTimestamp: string;
    
    // ILogFormatter 인터페이스 구현
    procedure SetSourceFormat(const Value: TSourceFormatInfo);
    function GetSourceFormat: TSourceFormatInfo;
    procedure SetSourceInfo(const Value: TSourceInfo);
    function GetSourceInfo: TSourceInfo;
    
    // 내부 헬퍼 메서드
    function GetFormattedTimestamp: string;
    function GetSourceIdentifierString: string;
    function FormatCustomString(const FormatStr: string): string;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // 초기화 메서드
    procedure CollectSourceInfo;
    
    // ILogFormatter 인터페이스 구현
    function FormatLogMessage(const Level: TLogLevel; const Msg: string; 
      const Tag: string = ''): string;
      
    // 속성
    property SourceFormat: TSourceFormatInfo read GetSourceFormat write SetSourceFormat;
    property SourceInfo: TSourceInfo read GetSourceInfo write SetSourceInfo;
    
    // 추가 속성
    property IdentifierFormat: TSourceIdentifierFormat read FIdentifierFormat write FIdentifierFormat;
    property CustomFormat: string read FCustomFormat write FCustomFormat;
  end;

implementation

uses
  blcksock
  
{$IFDEF MSWINDOWS}
  ,Windows
{$ENDIF}

{$IFDEF UNIX}
  ,BaseUnix, Unix, NetDB
{$ENDIF}
;

constructor TLogFormatter.Create;
begin
  inherited Create;
  
  FLock := SyncObjs.TCriticalSection.Create;
  
  // 소스 포맷 기본값 설정
  FSourceFormat.DisplayMode := sdmBoth;
  FSourceFormat.Prefix := '[';
  FSourceFormat.Suffix := ']';
  FSourceFormat.Separator := ':';
  FSourceFormat.SourceFirst := True;
  FSourceFormat.UseColor := False;
  
  // 소스 식별자 기본값 설정
  FIdentifierFormat := sfHostAndApp;
  FCustomFormat := '[%HOST%][%APP%]';
  
  // 시간 캐싱 초기화
  FLastTimestamp := 0;
  FFormattedTimestamp := '';
  
  // 소스 정보 수집
  CollectSourceInfo;
end;

destructor TLogFormatter.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TLogFormatter.SetSourceFormat(const Value: TSourceFormatInfo);
begin
  FLock.Enter;
  try
    FSourceFormat := Value;
  finally
    FLock.Leave;
  end;
end;

function TLogFormatter.GetSourceFormat: TSourceFormatInfo;
begin
  FLock.Enter;
  try
    Result := FSourceFormat;
  finally
    FLock.Leave;
  end;
end;

procedure TLogFormatter.SetSourceInfo(const Value: TSourceInfo);
begin
  FLock.Enter;
  try
    FSourceInfo := Value;
  finally
    FLock.Leave;
  end;
end;

function TLogFormatter.GetSourceInfo: TSourceInfo;
begin
  FLock.Enter;
  try
    Result := FSourceInfo;
  finally
    FLock.Leave;
  end;
end;

function TLogFormatter.GetFormattedTimestamp: string;
var
  CurrentTime: TDateTime;
begin
  CurrentTime := Now;

  // 캐시된 시간이 1초 이내면 재사용
  if (CurrentTime - FLastTimestamp) < (1 / (24 * 60 * 60)) then
    Result := FFormattedTimestamp
  else
  begin
    // 새로 포맷팅
    FLastTimestamp := CurrentTime;
    FFormattedTimestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', CurrentTime);
    Result := FFormattedTimestamp;
  end;
end;

procedure TLogFormatter.CollectSourceInfo;
var
  Socket: TTCPBlockSocket;
  {$IFDEF MSWINDOWS}
  Buffer: array[0..255] of Char;
  Len: DWORD;
  GuidValue: TGUID;
  {$ENDIF}
  {$IFDEF UNIX}
  Buffer: array[0..255] of Char;
  Info: utsname;
  Passwd: PPasswordRecord;
  {$ENDIF}
begin
  // 기본값 초기화
  FSourceInfo.HostName := 'unknown-host';
  FSourceInfo.IPAddress := '127.0.0.1';
  FSourceInfo.UserName := 'unknown-user';
  FSourceInfo.ApplicationName := ExtractFileName(ParamStr(0));
  FSourceInfo.ApplicationVersion := '1.0.0';

  try
    // Synapse를 사용하여 호스트명과 IP 주소 가져오기
    Socket := TTCPBlockSocket.Create;
    try
      try
        // 호스트 이름 가져오기
        FSourceInfo.HostName := Socket.LocalName;
        if FSourceInfo.HostName = '' then
          FSourceInfo.HostName := 'unknown-host';

        // IP 주소 가져오기
        FSourceInfo.IPAddress := Socket.ResolveName(FSourceInfo.HostName);
        if FSourceInfo.IPAddress = '' then
          FSourceInfo.IPAddress := '127.0.0.1';
      except
        FSourceInfo.HostName := 'unknown-host';
        FSourceInfo.IPAddress := '127.0.0.1';
      end;
    finally
      Socket.Free;
    end;

    {$IFDEF MSWINDOWS}
    // 사용자 이름 가져오기
    FillChar(Buffer, SizeOf(Buffer), 0);
    Len := GetEnvironmentVariable('USERNAME', Buffer, SizeOf(Buffer));
    if Len > 0 then
      FSourceInfo.UserName := Buffer
    else
      FSourceInfo.UserName := 'unknown-user';

    // 프로세스 ID 설정
    FSourceInfo.ProcessID := GetCurrentProcessId;

    // 고유 식별자 생성
    if CreateGUID(GuidValue) = S_OK then
      FSourceInfo.UniqueInstanceID := GUIDToString(GuidValue)
    else 
    begin
      Randomize;
      FSourceInfo.UniqueInstanceID := Format('%s-%d-%d',
        [FormatDateTime('yyyymmddhhnnsszzz', Now),
         FSourceInfo.ProcessID,
         Random(1000000)]);
    end;
    {$ENDIF}

    {$IFDEF UNIX}
    // Unix 시스템 정보
    if fpUname(Info) = 0 then
      FSourceInfo.HostName := Info.nodename;

    // 사용자 이름
    FSourceInfo.UserName := GetEnvironmentVariable('USER');
    if FSourceInfo.UserName = '' then 
    begin
      Passwd := getpwuid(fpGetUID);
      if Passwd <> nil then
        FSourceInfo.UserName := Passwd^.pw_name
      else
        FSourceInfo.UserName := 'unknown-user';
    end;

    // 프로세스 ID
    FSourceInfo.ProcessID := fpGetPID;

    // 고유 식별자 생성
    Randomize;
    FSourceInfo.UniqueInstanceID := Format('%s-%d-%d',
      [FormatDateTime('yyyymmddhhnnsszzz', Now),
       FSourceInfo.ProcessID,
       Random(1000000)]);
    {$ENDIF}
  except
    on E: Exception do
    begin
      // 소스 정보 수집 중 예외 발생 시 기본값 유지
      // 로그는 아직 설정되지 않았으므로 콘솔에 출력
      WriteLn('소스 정보 수집 중 오류: ', E.Message);
    end;
  end;
end;

function TLogFormatter.GetSourceIdentifierString: string;
begin
  // 포맷에 따라 소스 식별자 문자열 생성
  case FIdentifierFormat of
    sfHostName:
      Result := Format('[%s]', [FSourceInfo.HostName]);

    sfIPAddress:
      Result := Format('[%s]', [FSourceInfo.IPAddress]);

    sfApplication:
      Result := Format('[%s %s]', [FSourceInfo.ApplicationName, FSourceInfo.ApplicationVersion]);

    sfHostAndApp:
      Result := Format('[%s][%s]', [FSourceInfo.HostName, FSourceInfo.ApplicationName]);

    sfIPAndApp:
      Result := Format('[%s][%s]', [FSourceInfo.IPAddress, FSourceInfo.ApplicationName]);

    sfFull:
      Result := Format('[%s][%s][%s %s][%s][%d]',
                       [FSourceInfo.HostName,
                        FSourceInfo.IPAddress,
                        FSourceInfo.ApplicationName,
                        FSourceInfo.ApplicationVersion,
                        FSourceInfo.UserName,
                        FSourceInfo.ProcessID]);

    sfCustom:
      Result := FormatCustomString(FCustomFormat);
      
    else
      Result := '';
  end;
end;

function TLogFormatter.FormatCustomString(const FormatStr: string): string;
var
  TempStr: string;
begin
  TempStr := FormatStr;

  // 포맷 문자열 내 변수 치환
  TempStr := StringReplace(TempStr, '%HOST%', FSourceInfo.HostName, [rfReplaceAll, rfIgnoreCase]);
  TempStr := StringReplace(TempStr, '%IP%', FSourceInfo.IPAddress, [rfReplaceAll, rfIgnoreCase]);
  TempStr := StringReplace(TempStr, '%APP%', FSourceInfo.ApplicationName, [rfReplaceAll, rfIgnoreCase]);
  TempStr := StringReplace(TempStr, '%VER%', FSourceInfo.ApplicationVersion, [rfReplaceAll, rfIgnoreCase]);
  TempStr := StringReplace(TempStr, '%USER%', FSourceInfo.UserName, [rfReplaceAll, rfIgnoreCase]);
  TempStr := StringReplace(TempStr, '%PID%', IntToStr(FSourceInfo.ProcessID), [rfReplaceAll, rfIgnoreCase]);
  TempStr := StringReplace(TempStr, '%ID%', FSourceInfo.UniqueInstanceID, [rfReplaceAll, rfIgnoreCase]);

  Result := TempStr;
end;

function TLogFormatter.FormatLogMessage(const Level: TLogLevel; const Msg: string; 
  const Tag: string = ''): string;
var
  LevelStr: string;
  SourceIdStr: string;
  TimeStr: string;
  SourceIdentifier: string;
  TagStr: string;
begin
  // 로그 레벨 문자열 변환
  LevelStr := LogLevelToStr(Level);

  // 시간 문자열 (캐싱 사용)
  TimeStr := GetFormattedTimestamp;

  // 소스 식별자 처리
  case FSourceFormat.DisplayMode of
    sdmNone:
      SourceIdStr := '';

    sdmHandlerOnly:
      if Tag <> '' then
        SourceIdStr := FSourceFormat.Prefix + Tag + FSourceFormat.Suffix + ' '
      else
        SourceIdStr := '';

    sdmTagOnly:
      if Tag <> '' then
        SourceIdStr := FSourceFormat.Prefix + Tag + FSourceFormat.Suffix + ' '
      else
        SourceIdStr := '';

    sdmBoth:
      begin
        // 소스 식별자 가져오기
        SourceIdentifier := GetSourceIdentifierString;
        
        if (Tag <> '') then
        begin
          // 태그가 있는 경우
          if SourceIdentifier <> '' then
          begin
            if FSourceFormat.SourceFirst then
              SourceIdStr := SourceIdentifier + ' ' + 
                             FSourceFormat.Prefix + Tag + FSourceFormat.Suffix + ' '
            else
              SourceIdStr := FSourceFormat.Prefix + Tag + FSourceFormat.Suffix + ' ' + 
                             SourceIdentifier + ' ';
          end
          else
            SourceIdStr := FSourceFormat.Prefix + Tag + FSourceFormat.Suffix + ' ';
        end
        else
          SourceIdStr := SourceIdentifier + ' ';
      end;

    sdmFull:
      begin
        // 호스트/앱 식별자
        SourceIdentifier := GetSourceIdentifierString;
        
        // 태그 처리
        if Tag <> '' then
        begin
          if SourceIdentifier <> '' then
            SourceIdStr := SourceIdentifier + ' ' + 
                         FSourceFormat.Prefix + Tag + FSourceFormat.Suffix + ' '
          else
            SourceIdStr := FSourceFormat.Prefix + Tag + FSourceFormat.Suffix + ' ';
        end
        else
          SourceIdStr := SourceIdentifier + ' ';
      end;
      
    else
      SourceIdStr := '';
  end;

  // 최종 형식화된 메시지 생성
  Result := Format('[%s] [%s] %s%s',
                 [TimeStr,
                  LevelStr,
                  SourceIdStr,
                  Msg]);
end;

end.
