unit LogViewerForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DBGrids, DB, Buttons, ComCtrls, Menus, Clipbrd, CheckLst, Spin,
  LCLType,
  ZConnection, ZDataset, ZSqlUpdate, RxDBGrid, RxSortZeos, RxDBGridExportSpreadSheet,
  DbCtrls, DateUtils, DateTimePicker,
  LoggerBase, DatabaseStorage, BaseHandler, LogStorage, McJSON;

// 이 전역 변수를 추가합니다 - 리소스 없이 파생된 폼을 만들 수 있게 합니다
{$IFDEF FPC}
  {$PUSH}
  {$WARN 5024 OFF : Parameter "$1" not used}
{$ENDIF}
var
  RequireDerivedFormResource: Boolean = False;
{$IFDEF FPC}
  {$POP}
{$ENDIF}

type
  // 검색 기록을 저장하기 위한 레코드 타입
  TSearchHistory = record
    SearchText: string;
    LogLevel: string;
    DateFrom: TDateTime;
    DateTo: TDateTime;
  end;

  // 로그 뷰어 폼 클래스
  { TfrmLogViewer }
  TfrmLogViewer = class(TForm)
  private
    // UI 컴포넌트
    btnRefresh: TButton;
    btnClearLog: TButton;
    btnSearchClear: TButton;
    btnAutoRefresh: TSpeedButton;
    btnSaveSearch: TButton;
    btnLoadSearch: TButton;
    btnExportJson: TButton;
    btnImportJson: TButton;
    cbLogLevel: TComboBox;
    clbLogLevels: TCheckListBox;
    chkAutoScroll: TCheckBox;
    chkMultiLevel: TCheckBox;
    DataSource: TDataSource;
    DateFrom: TDateTimePicker;
    DateTo: TDateTimePicker;
    edtSearch: TEdit;
    edtRefreshInterval: TSpinEdit;
    gbFilter: TGroupBox;
    gbRefresh: TGroupBox;
    lblCount: TLabel;
    lblDate: TLabel;
    lblDateTo: TLabel;
    lblLevel: TLabel;
    lblSearch: TLabel;
    lblRefreshInterval: TLabel;
    LogConnection: TZConnection;
    LogQuery: TZQuery;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    miExportCSV: TMenuItem;
    miExportXLS: TMenuItem;
    miExportJSON: TMenuItem;
    miImportJSON: TMenuItem;
    miExit: TMenuItem;
    miDetailLog: TMenuItem;
    miFilteredDelete: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    RxDBGrid: TRxDBGrid;
    RxDBGridExportSpreadSheet: TRxDBGridExportSpreadSheet;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    TimerRefresh: TTimer;
    miFindDlg: TMenuItem;
    miColumnsDlg: TMenuItem;
    miFilterDlg: TMenuItem;
    miSortDlg: TMenuItem;
    miOptimizeColumns: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;

    // 레벨 필터 체크박스들
    chkINFO: TCheckBox;
    chkDEBUG: TCheckBox;
    chkWARN: TCheckBox;
    chkERROR: TCheckBox;
    chkFATAL: TCheckBox;
    chkDEVEL: TCheckBox;

    // 비공개 필드
    FAutoRefresh: Boolean;
    FLastPosition: Integer;
    FClearingFilter: Boolean;
    FSearchHistory: array of TSearchHistory;
    FLoading: Boolean;

    FDBStorage: TDatabaseLogStorage;

    // UI 컴포넌트 초기화 및 설정
    procedure CreateComponents;
    procedure SetupGrid;
    procedure SetupPopupMenu;
    procedure ApplyTheme;

    // 데이터 관련 메소드
    procedure LogLevelCheckClick(Sender: TObject);
    procedure RefreshData;
    procedure ApplyFilter;
    procedure ExportToCSV(const FileName: string);
    procedure ExportToXLS(const FileName: string);
    procedure ExportToJSON(const FileName: string);
    procedure ImportFromJSON(const FileName: string);
    procedure UpdateRowCount;
    procedure LoadSearchHistory;
    procedure SaveSearchHistory;

    // 이벤트 핸들러
    procedure btnAutoRefreshClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnSearchClearClick(Sender: TObject);
    procedure btnSaveSearchClick(Sender: TObject);
    procedure btnLoadSearchClick(Sender: TObject);
    procedure btnExportJsonClick(Sender: TObject);
    procedure btnImportJsonClick(Sender: TObject);
    procedure cbLogLevelChange(Sender: TObject);
    procedure chkMultiLevelClick(Sender: TObject);
    procedure clbLogLevelsClickCheck(Sender: TObject);
    procedure DateFromChange(Sender: TObject);
    procedure DateToChange(Sender: TObject);
    procedure edtRefreshIntervalChange(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure miExportCSVClick(Sender: TObject);
    procedure miExportXLSClick(Sender: TObject);
    procedure miExportJSONClick(Sender: TObject);
    procedure miImportJSONClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miDetailLogClick(Sender: TObject);
    procedure miFilteredDeleteClick(Sender: TObject);
    procedure RxDBGridGetCellProps(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor);
    procedure RxDBGridDblClick(Sender: TObject);
    procedure TimerRefreshTimer(Sender: TObject);

    // 팝업 메뉴 이벤트 핸들러
    procedure miFindDlgClick(Sender: TObject);
    procedure miColumnsDlgClick(Sender: TObject);
    procedure miFilterDlgClick(Sender: TObject);
    procedure miSortDlgClick(Sender: TObject);
    procedure miOptimizeColumnsClick(Sender: TObject);

    // 유틸리티 메소드
    procedure ShowDetailLog;
    procedure DeleteFilteredLogs;
    procedure InitializeDBStorage;
    
    // JSON 처리 메서드
    function GetSelectedLevels: TStringList;
    procedure SaveLogsToJSON(const FileName: string);
    procedure LoadLogsFromJSON(const FileName: string);

  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateWithDBStorage(AOwner: TComponent; ADBStorage: TDatabaseLogStorage);
    destructor Destroy; override;

    property DBStorage: TDatabaseLogStorage read FDBStorage write FDBStorage;
  end;

var
  frmLogViewer: TfrmLogViewer;

function SelectFromList(const Title, Prompt: string; Items: TStrings): Integer;

implementation

//{$R *.lfm}

uses
  GlobalLogger, StrUtils, IniFiles;

{ TfrmLogViewer }

constructor TfrmLogViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FLoading := True;
  
  try
    // UI 컴포넌트 생성 및 설정
    CreateComponents;

    // 폼 설정
    Caption := '로그 뷰어';
    Width := 820;  // 더 넓게 설정
    Height := 550; // 더 높게 설정
    Position := poScreenCenter;
    KeyPreview := True;  // 키 이벤트를 폼에서 처리

    OnCreate := @FormCreate;
    OnDestroy := @FormDestroy;
    OnClose := @FormClose;
    OnShow := @FormShow;
    OnKeyDown := @FormKeyDown;

    // 그리드 설정
    SetupGrid;
    // PopupMenu 설정
    SetupPopupMenu;

    // 타이머 설정
    FAutoRefresh := False;
    btnAutoRefresh.Down := False;
    TimerRefresh.Enabled := False;
    TimerRefresh.Interval := StrToIntDef(edtRefreshInterval.Text, 5) * 1000;

    // 검색 기록 로드
    LoadSearchHistory;
  finally
    FLoading := False;
  end;
end;

constructor TfrmLogViewer.CreateWithDBStorage(AOwner: TComponent; ADBStorage: TDatabaseLogStorage);
begin
  Create(AOwner);
  FDBStorage := ADBStorage;
  RefreshData;
end;

destructor TfrmLogViewer.Destroy;
begin
  try
    // 타이머 비활성화
    TimerRefresh.Enabled := False;

    // 데이터베이스 연결 닫기
    if LogQuery <> nil then
      LogQuery.Close;
    if LogConnection <> nil then
      LogConnection.Disconnect;

    // 검색 기록 저장
    SaveSearchHistory;
  except
    // 소멸자에서 예외가 발생하더라도 무시하고 진행
    on E: Exception do
      ; // 아무 작업 안함
  end;

  inherited Destroy;
end;

procedure TfrmLogViewer.FormCreate(Sender: TObject);
begin
  // 데이터베이스 저장소가 설정되지 않은 경우 초기화
  if not Assigned(FDBStorage) then
    InitializeDBStorage;
end;

procedure TfrmLogViewer.FormDestroy(Sender: TObject);
begin
  // DB 저장소 해제는 외부 연결일 수 있으므로 처리하지 않음
end;

procedure TfrmLogViewer.InitializeDBStorage;
var
  AppPath: string;
  DbPath: string;
  LogDbFile: string;
begin
  try
    // 애플리케이션 경로와 DB 경로 설정
    AppPath := ExtractFilePath(ParamStr(0));
    DbPath := IncludeTrailingPathDelimiter(AppPath) + 'logs';
    
    // 로그 디렉토리가 없으면 생성
    if not DirectoryExists(DbPath) then
    begin
      try
        ForceDirectories(DbPath);
      except
        on E: Exception do
        begin
          ShowMessage('로그 디렉토리 생성 실패: ' + E.Message);
          // 현재 디렉토리로 대체
          DbPath := AppPath;
        end;
      end;
    end;

    // 기본 로그 DB 파일
    LogDbFile := IncludeTrailingPathDelimiter(DbPath) + 'Log.fdb';
    
    // DB 연결 설정
    LogConnection.Protocol := 'firebird';
    LogConnection.ClientCodepage := 'UTF8';
    LogConnection.LibraryLocation := AppPath + 'fbclient.dll';
    LogConnection.Database := LogDbFile;
    LogConnection.User := 'SYSDBA';
    LogConnection.Password := 'masterkey';

    try
      LogConnection.Connect;
      
      // DB 저장소 생성
      FDBStorage := TDatabaseLogStorage.Create('localhost', LogDbFile, 'SYSDBA', 'masterkey');
      FDBStorage.Init;
    except
      on E: Exception do
      begin
        ShowMessage('로그 데이터베이스 연결 오류: ' + E.Message);
      end;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('데이터베이스 초기화 오류: ' + E.Message);
    end;
  end;
end;

procedure TfrmLogViewer.CreateComponents;
begin
  try
    // === 패널 및 그룹박스 ===
    Panel1 := TPanel.Create(Self);
    Panel1.Parent := Self;
    Panel1.Align := alTop;
    Panel1.Height := 116;
    Panel1.BevelOuter := bvNone;

    Panel2 := TPanel.Create(Self);
    Panel2.Parent := Self;
    Panel2.Align := alBottom;
    Panel2.Height := 25;
    Panel2.BevelOuter := bvNone;
    Panel2.Font.Name:= 'Arial';

    gbFilter := TGroupBox.Create(Self);
    gbFilter.Parent := Panel1;
    gbFilter.Caption := '필터';
    gbFilter.Left := 8;
    gbFilter.Top := 8;
    gbFilter.Width := 518;
    gbFilter.Height := 106;

    gbRefresh := TGroupBox.Create(Self);
    gbRefresh.Parent := Panel1;
    gbRefresh.Caption := '새로고침 설정';
    gbRefresh.Left := 534;
    gbRefresh.Top := 8;
    gbRefresh.Width := 270;
    gbRefresh.Height := 106;

    // === 체크박스 (레벨 필터) - 첫 번째 행에 배치 ===
    chkINFO := TCheckBox.Create(Self);
    chkINFO.Parent := gbFilter;
    chkINFO.Caption := 'INFO';
    chkINFO.Left := 11;
    chkINFO.Top := 8;
    chkINFO.Width := 48;
    chkINFO.Checked := True;
    chkINFO.OnClick := @LogLevelCheckClick;

    chkDEBUG := TCheckBox.Create(Self);
    chkDEBUG.Parent := gbFilter;
    chkDEBUG.Caption := 'DEBUG';
    chkDEBUG.Left := 71;
    chkDEBUG.Top := 8;
    chkDEBUG.Width := 58;
    chkDEBUG.Checked := True;
    chkDEBUG.OnClick := @LogLevelCheckClick;

    chkWARN := TCheckBox.Create(Self);
    chkWARN.Parent := gbFilter;
    chkWARN.Caption := 'WARN';
    chkWARN.Left := 141;
    chkWARN.Top := 8;
    chkWARN.Width := 53;
    chkWARN.Checked := True;
    chkWARN.OnClick := @LogLevelCheckClick;

    chkERROR := TCheckBox.Create(Self);
    chkERROR.Parent := gbFilter;
    chkERROR.Caption := 'ERROR';
    chkERROR.Left := 201;
    chkERROR.Top := 8;
    chkERROR.Width := 60;
    chkERROR.Checked := True;
    chkERROR.OnClick := @LogLevelCheckClick;

    chkFATAL := TCheckBox.Create(Self);
    chkFATAL.Parent := gbFilter;
    chkFATAL.Caption := 'FATAL';
    chkFATAL.Left := 266;
    chkFATAL.Top := 8;
    chkFATAL.Width := 60;
    chkFATAL.Checked := True;
    chkFATAL.OnClick := @LogLevelCheckClick;

    chkDEVEL := TCheckBox.Create(Self);
    chkDEVEL.Parent := gbFilter;
    chkDEVEL.Caption := 'DEVEL';
    chkDEVEL.Left := 331;
    chkDEVEL.Top := 8;
    chkDEVEL.Width := 60;
    chkDEVEL.Checked := True;
    chkDEVEL.OnClick := @LogLevelCheckClick;

    // === 레이블 ===
    lblDate := TLabel.Create(Self);
    lblDate.Parent := gbFilter;
    lblDate.Caption := '시작일';
    lblDate.Left := 11;
    lblDate.Top := 38;

    lblDateTo := TLabel.Create(Self);
    lblDateTo.Parent := gbFilter;
    lblDateTo.Caption := '종료일';
    lblDateTo.Left := 104;
    lblDateTo.Top := 38;

    lblSearch := TLabel.Create(Self);
    lblSearch.Parent := gbFilter;
    lblSearch.Caption := '검색어';
    lblSearch.Left := 210;
    lblSearch.Top := 38;

    lblCount := TLabel.Create(Self);
    lblCount.Parent := Panel2;
    lblCount.Caption := 'ShowQuickFilter:(Ctrl+Shift+Q)   HideQuickFilter:(Ctrl+Shift+H)';
    lblCount.Left := 8;
    lblCount.Top := 5;

    lblRefreshInterval := TLabel.Create(Self);
    lblRefreshInterval.Parent := gbRefresh;
    lblRefreshInterval.Caption := '새로고침 간격(초):';
    lblRefreshInterval.Left := 10;
    lblRefreshInterval.Top := 8;

    // 날짜 선택기 설정
    DateFrom := TDateTimePicker.Create(Self);
    DateFrom.Parent := gbFilter;
    DateFrom.Left := 11;
    DateFrom.Top := 58;
    DateFrom.Width := 83;
    DateFrom.Date := Date - 7; // 기본값으로 1주일 전
    DateFrom.OnChange := @DateFromChange;

    DateTo := TDateTimePicker.Create(Self);
    DateTo.Parent := gbFilter;
    DateTo.Left := 104;
    DateTo.Top := 58;
    DateTo.Width := 83;
    DateTo.Date := Date;
    DateTo.OnChange := @DateToChange;

    // 검색창 설정
    edtSearch := TEdit.Create(Self);
    edtSearch.Parent := gbFilter;
    edtSearch.Left := 210;
    edtSearch.Top := 58;
    edtSearch.Width := 200;
    edtSearch.OnChange := @edtSearchChange;

    // 검색 관련 버튼 - 수직 배치
    btnLoadSearch := TButton.Create(Self);
    btnLoadSearch.Parent := gbFilter;
    btnLoadSearch.Caption := '검색 불러오기';
    btnLoadSearch.Left := 419;
    btnLoadSearch.Top := 6;
    btnLoadSearch.Width := 88;
    btnLoadSearch.Height := 23;
    btnLoadSearch.OnClick := @btnLoadSearchClick;

    btnSaveSearch := TButton.Create(Self);
    btnSaveSearch.Parent := gbFilter;
    btnSaveSearch.Caption := '검색 저장';
    btnSaveSearch.Left := 419;
    btnSaveSearch.Top := 32;
    btnSaveSearch.Width := 88;
    btnSaveSearch.Height := 23;
    btnSaveSearch.OnClick := @btnSaveSearchClick;

    btnSearchClear := TButton.Create(Self);
    btnSearchClear.Parent := gbFilter;
    btnSearchClear.Caption := '지우기';
    btnSearchClear.Left := 419;
    btnSearchClear.Top := 58;
    btnSearchClear.Width := 88;
    btnSearchClear.Height := 23;
    btnSearchClear.OnClick := @btnSearchClearClick;

    // JSON 관련 버튼 추가
    btnExportJson := TButton.Create(Self);
    btnExportJson.Parent := gbFilter;
    btnExportJson.Caption := 'JSON 내보내기';
    btnExportJson.Left := 419;
    btnExportJson.Top := 82;
    btnExportJson.Width := 88;
    btnExportJson.Height := 23;
    btnExportJson.OnClick := @btnExportJsonClick;

    // 자동 스크롤 체크박스
    chkAutoScroll := TCheckBox.Create(Self);
    chkAutoScroll.Parent := gbRefresh;
    chkAutoScroll.Caption := '최신 로그를 상단에 표시';
    chkAutoScroll.Left := 10;
    chkAutoScroll.Top := 32;
    chkAutoScroll.Width := 180;
    chkAutoScroll.Checked := True;

    // 새로고침 간격 설정
    edtRefreshInterval := TSpinEdit.Create(Self);
    edtRefreshInterval.Parent := gbRefresh;
    edtRefreshInterval.Left := 120;
    edtRefreshInterval.Top := 6;
    edtRefreshInterval.Width := 60;
    edtRefreshInterval.MinValue := 1;
    edtRefreshInterval.MaxValue := 60;
    edtRefreshInterval.Value := 5;
    edtRefreshInterval.OnChange := @edtRefreshIntervalChange;

    // === 버튼 설정 ===
    btnRefresh := TButton.Create(Self);
    btnRefresh.Parent := gbRefresh;
    btnRefresh.Caption := '새로고침';
    btnRefresh.Left := 10;
    btnRefresh.Top := 58;
    btnRefresh.Width := 70;
    btnRefresh.Height := 25;
    btnRefresh.OnClick := @btnRefreshClick;

    btnAutoRefresh := TSpeedButton.Create(Self);
    btnAutoRefresh.Parent := gbRefresh;
    btnAutoRefresh.Caption := '자동새로고침';
    btnAutoRefresh.Left := 85;
    btnAutoRefresh.Top := 58;
    btnAutoRefresh.Width := 90;
    btnAutoRefresh.Height := 25;
    btnAutoRefresh.AllowAllUp := True;
    btnAutoRefresh.GroupIndex := 1;
    btnAutoRefresh.OnClick := @btnAutoRefreshClick;

    btnClearLog := TButton.Create(Self);
    btnClearLog.Parent := gbRefresh;
    btnClearLog.Caption := '로그 지우기';
    btnClearLog.Left := 180;
    btnClearLog.Top := 58;
    btnClearLog.Width := 80;
    btnClearLog.Height := 25;
    btnClearLog.OnClick := @btnClearLogClick;

    // JSON 가져오기 버튼
    btnImportJson := TButton.Create(Self);
    btnImportJson.Parent := gbRefresh;
    btnImportJson.Caption := 'JSON 가져오기';
    btnImportJson.Left := 180;
    btnImportJson.Top := 30;
    btnImportJson.Width := 80;
    btnImportJson.Height := 25;
    btnImportJson.OnClick := @btnImportJsonClick;

    // === 데이터 컴포넌트 ===
    RxDBGrid := TRxDBGrid.Create(Self);
    RxDBGrid.Parent := Self;
    RxDBGrid.Align := alClient;
    RxDBGrid.OnGetCellProps := @RxDBGridGetCellProps;
    RxDBGrid.OnDblClick := @RxDBGridDblClick;
    RxDBGrid.TitleButtons := True;
    RxDBGrid.ReadOnly := True;
    RxDBGrid.FooterOptions.Active := True;
    RxDBGrid.DrawFullLine := True;

    RxDBGridExportSpreadSheet := TRxDBGridExportSpreadSheet.Create(Self);

    LogConnection := TZConnection.Create(Self);
    LogQuery := TZQuery.Create(Self);
    DataSource := TDataSource.Create(Self);

    // === 메뉴 및 다이얼로그 ===
    MainMenu := TMainMenu.Create(Self);
    PopupMenu := TPopupMenu.Create(Self);
    RxDBGrid.PopupMenu := PopupMenu;

    MenuItem1 := TMenuItem.Create(Self);
    MenuItem1.Caption := '파일';
    MainMenu.Items.Add(MenuItem1);

    miExportCSV := TMenuItem.Create(Self);
    miExportCSV.Caption := 'CSV 내보내기...';
    miExportCSV.OnClick := @miExportCSVClick;
    MenuItem1.Add(miExportCSV);

    miExportXLS := TMenuItem.Create(Self);
    miExportXLS.Caption := 'Excel 내보내기...';
    miExportXLS.OnClick := @miExportXLSClick;
    MenuItem1.Add(miExportXLS);

    miExportJSON := TMenuItem.Create(Self);
    miExportJSON.Caption := 'JSON 내보내기...';
    miExportJSON.OnClick := @miExportJSONClick;
    MenuItem1.Add(miExportJSON);

    miImportJSON := TMenuItem.Create(Self);
    miImportJSON.Caption := 'JSON 가져오기...';
    miImportJSON.OnClick := @miImportJSONClick;
    MenuItem1.Add(miImportJSON);

    N3 := TMenuItem.Create(Self);
    N3.Caption := '-';
    MenuItem1.Add(N3);

    N2 := TMenuItem.Create(Self);
    N2.Caption := '-';
    MenuItem1.Add(N2);

    miDetailLog := TMenuItem.Create(Self);
    miDetailLog.Caption := '로그 상세 보기';
    miDetailLog.ShortCut := ShortCut(VK_F3, []);
    miDetailLog.OnClick := @miDetailLogClick;
    MenuItem1.Add(miDetailLog);

    miFilteredDelete := TMenuItem.Create(Self);
    miFilteredDelete.Caption := '필터된 로그 삭제';
    miFilteredDelete.OnClick := @miFilteredDeleteClick;
    MenuItem1.Add(miFilteredDelete);

    N1 := TMenuItem.Create(Self);
    N1.Caption := '-';
    MenuItem1.Add(N1);

    miExit := TMenuItem.Create(Self);
    miExit.Caption := '종료';
    miExit.OnClick := @miExitClick;
    MenuItem1.Add(miExit);

    SaveDialog := TSaveDialog.Create(Self);
    OpenDialog := TOpenDialog.Create(Self);

    TimerRefresh := TTimer.Create(Self);
    TimerRefresh.Enabled := False;
    TimerRefresh.OnTimer := @TimerRefreshTimer;

    // 데이터 연결 설정
    LogQuery.Connection := LogConnection;
    DataSource.DataSet := LogQuery;
    RxDBGrid.DataSource := DataSource;

    // 메뉴 설정
    Menu := MainMenu;
  except
    on E: Exception do
      ShowMessage('컴포넌트 초기화 중 오류가 발생했습니다: ' + E.Message);
  end;
end;

procedure TfrmLogViewer.SetupGrid;
var
  Column: TRxColumn;
begin
  with RxDBGrid do
  begin
    // 기존 열 모두 제거
    Columns.Clear;

    // ID 열 추가 (숨김)
    Column := TRxColumn.Create(Columns);
    Column.FieldName := 'ID';
    Column.Visible := False;

    // LDATE 열 추가
    Column := TRxColumn.Create(Columns);
    Column.FieldName := 'LDATE';
    Column.Alignment := taCenter;
    Column.Filter.AllValue := '*';
    Column.Title.Caption := '일자';
    Column.Title.Alignment := taCenter;
    Column.Title.Font.Style := [fsBold];
    Column.Width := 74;

    // LTIME 열 추가
    Column := TRxColumn.Create(Columns);
    Column.FieldName := 'LTIME';
    Column.Alignment := taCenter;
    Column.Filter.AllValue := '*';
    Column.Filter.Style := rxfstmanualEdit;
    Column.DisplayFormat := 'hh:nn:ss.zzz';
    Column.Title.Caption := '시간';
    Column.Title.Alignment := taCenter;
    Column.Title.Font.Style := [fsBold];
    Column.Width := 80;

    // LLEVEL 열 추가
    Column := TRxColumn.Create(Columns);
    Column.FieldName := 'LLEVEL';
    Column.Filter.AllValue := '*';
    Column.Title.Caption := '레벨';
    Column.Title.Alignment := taCenter;
    Column.Title.Font.Style := [fsBold];
    Column.Width := 70;

    // LSOURCE 열 추가
    Column := TRxColumn.Create(Columns);
    Column.FieldName := 'LSOURCE';
    Column.Filter.AllValue := '*';
    Column.Title.Caption := '소스';
    Column.Title.Alignment := taCenter;
    Column.Title.Font.Style := [fsBold];
    Column.Width := 90;

    // LMESSAGE 열 추가
    Column := TRxColumn.Create(Columns);
    Column.FieldName := 'LMESSAGE';
    Column.Filter.AllValue := '*';
    Column.Filter.Style := rxfstmanualEdit;
    Column.Title.Caption := '메시지';
    Column.Title.Alignment := taCenter;
    Column.Title.Font.Style := [fsBold];
    Column.Width := 400;

    // EXT_DATA_ID 열 추가 (숨김)
    Column := TRxColumn.Create(Columns);
    Column.FieldName := 'EXT_DATA_ID';
    Column.Visible := False;

    // RAW_JSON 열 추가 (숨김)
    Column := TRxColumn.Create(Columns);
    Column.FieldName := 'RAW_JSON';
    Column.Visible := False;

    // 기본 그리드 설정
    Options := [dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines,
                dgRowLines, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit,
                dgHeaderHotTracking, dgHeaderPushedLook, dgAnyButtonCanSelect,
                dgDisableDelete, dgDisableInsert, dgTruncCellHints, dgThumbTracking,
                dgDisplayMemoText, dgRowSelect, dgMultiselect];

    OptionsRx := [rdgAllowFilterForm, rdgAllowColumnsForm, rdgAllowDialogFind,
                  rdgAllowQuickFilter, rdgAllowQuickSearch, rdgAllowSortForm,
                  rdgAllowToolMenu, rdgFooterRows, rdgHighlightFocusCol, rdgMultiTitleLines];

    // 단축키 설정
    KeyStrokes[4].ShortCut := ShortCut(Ord('Q'), [ssCtrl, ssShift]);
    KeyStrokes[5].ShortCut := ShortCut(Ord('H'), [ssCtrl, ssShift]);

    // 확장 옵션 설정
    TitleButtons := True;  // 헤더 클릭으로 정렬

    // 푸터에 로그 개수 표시
    FooterOptions.Active := True;
    FooterOptions.RowCount := 1;
    Columns[1].Footer.ValueType := fvtCount;
    Columns[1].Footer.FieldName := 'LDATE';
  end;
end;

procedure TfrmLogViewer.SetupPopupMenu;
begin
  // 팝업 메뉴 항목 추가
  miFindDlg := TMenuItem.Create(PopupMenu);
  miFindDlg.Caption := '검색 대화상자 (Ctrl+F)';
  miFindDlg.ShortCut := ShortCut(Ord('F'), [ssCtrl]);
  miFindDlg.OnClick := @miFindDlgClick;
  PopupMenu.Items.Add(miFindDlg);

  miColumnsDlg := TMenuItem.Create(PopupMenu);
  miColumnsDlg.Caption := '열 설정 대화상자 (Ctrl+W)';
  miColumnsDlg.ShortCut := ShortCut(Ord('W'), [ssCtrl]);
  miColumnsDlg.OnClick := @miColumnsDlgClick;
  PopupMenu.Items.Add(miColumnsDlg);

  miFilterDlg := TMenuItem.Create(PopupMenu);
  miFilterDlg.Caption := '필터 대화상자 (Ctrl+T)';
  miFilterDlg.ShortCut := ShortCut(Ord('T'), [ssCtrl]);
  miFilterDlg.OnClick := @miFilterDlgClick;
  PopupMenu.Items.Add(miFilterDlg);

  miSortDlg := TMenuItem.Create(PopupMenu);
  miSortDlg.Caption := '정렬 대화상자 (Ctrl+S)';
  miSortDlg.ShortCut := ShortCut(Ord('S'), [ssCtrl]);
  miSortDlg.OnClick := @miSortDlgClick;
  PopupMenu.Items.Add(miSortDlg);

  miOptimizeColumns := TMenuItem.Create(PopupMenu);
  miOptimizeColumns.Caption := '컬럼 너비 최적화';
  miOptimizeColumns.OnClick := @miOptimizeColumnsClick;
  PopupMenu.Items.Add(miOptimizeColumns);

  // 구분선 추가
  N1 := TMenuItem.Create(PopupMenu);
  N1.Caption := '-';
  PopupMenu.Items.Add(N1);

  miExportXLS := TMenuItem.Create(PopupMenu);
  miExportXLS.Caption := 'Excel 내보내기 (Ctrl+X)';
  miExportXLS.ShortCut := ShortCut(Ord('X'), [ssCtrl]);
  miExportXLS.OnClick := @miExportXLSClick;
  PopupMenu.Items.Add(miExportXLS);

  // JSON 내보내기 메뉴 추가
  miExportJSON := TMenuItem.Create(PopupMenu);
  miExportJSON.Caption := 'JSON 내보내기';
  miExportJSON.OnClick := @miExportJSONClick;
  PopupMenu.Items.Add(miExportJSON);

  // JSON 가져오기 메뉴 추가
  miImportJSON := TMenuItem.Create(PopupMenu);
  miImportJSON.Caption := 'JSON 가져오기';
  miImportJSON.OnClick := @miImportJSONClick;
  PopupMenu.Items.Add(miImportJSON);
end;

procedure TfrmLogViewer.ApplyTheme;
begin
  // UI 테마 적용 (사용자 시스템 설정에 맞게 조정 가능)
  Color := clBtnFace;
  Font.Name := 'Tahoma';
  Font.Size := 9;

  // 그리드 색상 설정
  RxDBGrid.Color := clWindow;
  RxDBGrid.FooterOptions.Color := clBtnFace;
  RxDBGrid.TitleFont.Style := [fsBold];
end;

procedure TfrmLogViewer.FormShow(Sender: TObject);
begin
  RefreshData;
  Self.KeyPreview := True;
end;

procedure TfrmLogViewer.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // F3 키: 로그 상세 보기
  if Key = VK_F3 then
  begin
    ShowDetailLog;
    Key := 0; // 키 이벤트 소비
  end;
end;

procedure TfrmLogViewer.LogLevelCheckClick(Sender: TObject);
begin
  if not FClearingFilter then
    ApplyFilter;
end;

procedure TfrmLogViewer.RefreshData;
var
  LastID: Integer;
  ScrollToEnd: Boolean;
  SavedCursor: TCursor;
  TempQuery: TZQuery;
  SearchText: string;
  LevelFilter: string;
  SelectedLevels: TStringList;
begin
  SavedCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;  // 모래시계 커서 표시

  try
    // 현재 선택된 레코드의 ID 저장
    FLastPosition := -1;
    if not LogQuery.IsEmpty then
    begin
      FLastPosition := LogQuery.FieldByName('ID').AsInteger;
    end;

    // 자동 스크롤 설정 확인
    ScrollToEnd := chkAutoScroll.Checked;

    if not Assigned(FDBStorage) then
    begin
      // 데이터베이스 저장소가 없으면 초기화
      DebugToFile('TfrmLogViewer.RefreshData: FDBStorage가 할당되지 않았습니다.');
      InitializeDBStorage;
      
      if not Assigned(FDBStorage) then
      begin
        ShowMessage('데이터베이스 저장소를 초기화할 수 없습니다.');
        Exit;
      end;
    end;

    // 검색어
    SearchText := Trim(edtSearch.Text);
    
    // 로그 레벨 필터링 문자열 생성
    SelectedLevels := GetSelectedLevels;
    try
      if SelectedLevels.Count < 6 then // 모든 레벨을 선택한 경우가 아니면
      begin
        if SelectedLevels.Count > 0 then
          LevelFilter := SelectedLevels[0]
        else
          LevelFilter := ''; // 아무 레벨도 선택되지 않은 경우
      end
      else
        LevelFilter := ''; // 모든 레벨 선택 (필터 없음)
    finally
      SelectedLevels.Free;
    end;

    // 로그 데이터 로드
    if Assigned(FDBStorage) then
    begin
      try
        // 기존 쿼리 닫기
        if LogQuery.Active then
          LogQuery.Close;

        // DatabaseLogStorage의 GetLogs 메서드 사용하여 로그 조회
        TempQuery := FDBStorage.GetLogs(
          DateFrom.Date,       // 시작일
          DateTo.Date,         // 종료일
          LevelFilter,         // 로그 레벨 필터
          SearchText           // 검색어
        );

        // 쿼리 복사
        LogQuery.SQL.Text := TempQuery.SQL.Text;
        LogQuery.Params.Assign(TempQuery.Params);
        LogQuery.Open;

        // 임시 쿼리 해제
        TempQuery.Close;
        TempQuery.Free;
      except
        on E: Exception do
        begin
          ShowMessage('로그 데이터 로드 오류: ' + E.Message);
          
          // 기본 쿼리 실행 시도
          try
            LogQuery.Close;
            LogQuery.SQL.Text :=
              'SELECT ID, LDATE, LTIME, LLEVEL, LSOURCE, LMESSAGE, RAW_JSON, EXT_DATA_ID ' +
              'FROM ' + FDBStorage.CurrentMonthTable + ' ' +
              'ORDER BY LDATE DESC, LTIME DESC';
            LogQuery.Open;
          except
            on E2: Exception do
              ShowMessage('기본 쿼리 실행 오류: ' + E2.Message);
          end;
        end;
      end;
    end;

    // 로그 레벨 필터 적용
    ApplyFilter;

    // 마지막 위치로 이동 또는 맨 끝으로 이동
    if ScrollToEnd and (not LogQuery.IsEmpty) then
    begin
      LogQuery.First;
    end
    else if FLastPosition > 0 then
    begin
      LogQuery.Locate('ID', FLastPosition, []);
    end;

    // 로그 레코드 카운트 업데이트
    UpdateRowCount;
  finally
    Screen.Cursor := SavedCursor;  // 원래 커서로 복원
  end;
end;

function TfrmLogViewer.GetSelectedLevels: TStringList;
begin
  Result := TStringList.Create;
  
  if chkINFO.Checked then
    Result.Add('INFO');
  if chkDEBUG.Checked then
    Result.Add('DEBUG');
  if chkWARN.Checked then
    Result.Add('WARNING');
  if chkERROR.Checked then
    Result.Add('ERROR');
  if chkFATAL.Checked then
    Result.Add('FATAL');
  if chkDEVEL.Checked then
    Result.Add('DEVELOP');
end;

procedure TfrmLogViewer.ApplyFilter;
var
  FilterStr: string;
  SelectedLevels: TStringList;
  i: Integer;
begin
  FClearingFilter := True;
  try
    // 로그 레벨 필터 - 체크박스 방식
    SelectedLevels := GetSelectedLevels;
    try
      // 모든 로그 레벨이 선택된 경우 필터 해제
      if SelectedLevels.Count = 6 then
      begin
        LogQuery.Filtered := False;
      end
      else if SelectedLevels.Count > 0 then
      begin
        // 일부 로그 레벨만 선택된 경우
        FilterStr := '';
        for i := 0 to SelectedLevels.Count - 1 do
        begin
          if i > 0 then
            FilterStr := FilterStr + ' OR ';
          FilterStr := FilterStr + Format('LLEVEL = ''%s''', [SelectedLevels[i]]);
        end;

        // 필터 적용
        LogQuery.Filter := '(' + FilterStr + ')';
        LogQuery.Filtered := True;
      end
      else
      begin
        // 어떤 로그 레벨도 선택되지 않은 경우 (모든 로그 숨김)
        LogQuery.Filter := 'LLEVEL = ''NONE''';
        LogQuery.Filtered := True;
      end;
    finally
      SelectedLevels.Free;
    end;
  finally
    FClearingFilter := False;
    UpdateRowCount;
  end;
end;

procedure TfrmLogViewer.RxDBGridGetCellProps(Sender: TObject; Field: TField;
  AFont: TFont; var Background: TColor);
var
  Level: string;
begin
  if Field = nil then Exit;

  // 레벨에 따른 색상 설정
  if LogQuery.FieldByName('LLEVEL') <> nil then
  begin
    Level := LogQuery.FieldByName('LLEVEL').AsString;

    if Level = 'DEVELOP' then
      AFont.Color := clGray
    else if Level = 'DEBUG' then
      AFont.Color := clBlue
    else if Level = 'INFO' then
      AFont.Color := clBlack
    else if Level = 'WARNING' then
    begin
      AFont.Color := clMaroon;
      AFont.Style := [fsBold];
    end
    else if Level = 'ERROR' then
    begin
      AFont.Color := clRed;
      AFont.Style := [fsBold];
    end
    else if Level = 'FATAL' then
    begin
      AFont.Color := clRed;
      AFont.Style := [fsBold];
      Background := clYellow;
    end;
  end;
end;

procedure TfrmLogViewer.RxDBGridDblClick(Sender: TObject);
begin
  // 그리드 더블 클릭 시 로그 상세 보기
  ShowDetailLog;
end;

procedure TfrmLogViewer.btnRefreshClick(Sender: TObject);
begin
  RefreshData;
end;

procedure TfrmLogViewer.btnClearLogClick(Sender: TObject);
var
  LogTables: TStringList;
  i: Integer;
begin
  if MessageDlg('경고', '모든 로그 기록을 삭제하시겠습니까? 이 작업은 되돌릴 수 없습니다.',
     mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      // DatabaseHandler 얻기
      if not Assigned(FDBStorage) then
      begin
        // 기존 단일 테이블 방식
        LogQuery.Close;
        LogQuery.SQL.Text := 'DELETE FROM LOGS';
        LogQuery.ExecSQL;
      end
      else
      begin
        // 월별 테이블 방식
        LogTables := TStringList.Create;
        try
          LogQuery.Close;

          // 메타 테이블 비우기
          LogQuery.SQL.Text := 'DELETE FROM ' + FDBStorage.MetaTableName;
          LogQuery.ExecSQL;

          // 테이블 목록 조회
          LogQuery.SQL.Text :=
            'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
            'WHERE RDB$RELATION_NAME LIKE ''LOGS_%'' AND RDB$SYSTEM_FLAG = 0';
          LogQuery.Open;

          while not LogQuery.EOF do
          begin
            LogTables.Add(Trim(LogQuery.FieldByName('RDB$RELATION_NAME').AsString));
            LogQuery.Next;
          end;

          LogQuery.Close;

          // 모든 로그 테이블 삭제
          for i := 0 to LogTables.Count - 1 do
          begin
            try
              // 시퀀스 삭제
              LogQuery.SQL.Text := 'DROP SEQUENCE SEQ_' + LogTables[i];
              LogQuery.ExecSQL;

              // 테이블 삭제
              LogQuery.SQL.Text := 'DROP TABLE ' + LogTables[i];
              LogQuery.ExecSQL;
            except
              // 테이블 삭제 중 오류는 무시하고 계속 진행
              on E: Exception do
                ; // 무시
            end;
          end;
        finally
          LogTables.Free;
        end;

        // 현재 월 테이블 재생성 (DBStorage 초기화)
        FDBStorage.Init;
      end;

      RefreshData;
      ShowMessage('모든 로그가 삭제되었습니다.');
    except
      on E: Exception do
        ShowMessage('로그 삭제 중 오류가 발생했습니다: ' + E.Message);
    end;
  end;
end;

procedure TfrmLogViewer.btnAutoRefreshClick(Sender: TObject);
begin
  FAutoRefresh := btnAutoRefresh.Down;
  TimerRefresh.Enabled := FAutoRefresh;

  if FAutoRefresh then
    btnAutoRefresh.Caption := '자동 갱신 중...'
  else
    btnAutoRefresh.Caption := '자동 새로고침';
end;

procedure TfrmLogViewer.btnSearchClearClick(Sender: TObject);
begin
  edtSearch.Clear;

  // 체크박스 초기화 - 모두 선택
  chkINFO.Checked := True;
  chkDEBUG.Checked := True;
  chkWARN.Checked := True;
  chkERROR.Checked := True;
  chkFATAL.Checked := True;
  chkDEVEL.Checked := True;

  DateFrom.Date := Date - 7;
  DateTo.Date := Date;

  // 데이터 새로고침
  RefreshData;
end;

procedure TfrmLogViewer.btnSaveSearchClick(Sender: TObject);
var
  SearchName: string;
  NewSearch: TSearchHistory;
  SelectedLevels: TStringList;
begin
  SearchName := InputBox('검색 저장', '이 검색 조건의 이름을 입력하세요:', '');
  if SearchName = '' then Exit;

  // 로그 레벨 문자열 생성
  SelectedLevels := GetSelectedLevels;
  try
    // 새 검색 기록 생성
    NewSearch.SearchText := edtSearch.Text;
    NewSearch.LogLevel := SelectedLevels.CommaText;
    NewSearch.DateFrom := DateFrom.Date;
    NewSearch.DateTo := DateTo.Date;
  finally
    SelectedLevels.Free;
  end;

  // 배열에 추가
  SetLength(FSearchHistory, Length(FSearchHistory) + 1);
  FSearchHistory[High(FSearchHistory)] := NewSearch;

  // 파일에 저장
  SaveSearchHistory;

  ShowMessage('검색 조건이 저장되었습니다.');
end;

procedure TfrmLogViewer.btnLoadSearchClick(Sender: TObject);
var
  i: Integer;
  Items: TStringList;
  SelectedIdx: Integer;
  LevelsList: TStringList;
  LevelStr: string;
begin
  if Length(FSearchHistory) = 0 then
  begin
    ShowMessage('저장된 검색 조건이 없습니다.');
    Exit;
  end;

  Items := TStringList.Create;
  try
    for i := 0 to High(FSearchHistory) do
      Items.Add(Format('검색: %s, 레벨: %s, 날짜: %s - %s',
                  [FSearchHistory[i].SearchText,
                   FSearchHistory[i].LogLevel,
                   FormatDateTime('yyyy-mm-dd', FSearchHistory[i].DateFrom),
                   FormatDateTime('yyyy-mm-dd', FSearchHistory[i].DateTo)]));

    SelectedIdx := SelectFromList('검색 불러오기', '저장된 검색 조건을 선택하세요:', Items);
    if SelectedIdx >= 0 then
    begin
      // 선택한 검색 조건 로드
      edtSearch.Text := FSearchHistory[SelectedIdx].SearchText;
      
      // 레벨 체크박스 설정
      chkINFO.Checked := False;
      chkDEBUG.Checked := False;
      chkWARN.Checked := False;
      chkERROR.Checked := False;
      chkFATAL.Checked := False;
      chkDEVEL.Checked := False;
      
      LevelsList := TStringList.Create;
      try
        LevelsList.CommaText := FSearchHistory[SelectedIdx].LogLevel;
        
        for i := 0 to LevelsList.Count - 1 do
        begin
          LevelStr := LevelsList[i];
          
          if LevelStr = 'INFO' then
            chkINFO.Checked := True
          else if LevelStr = 'DEBUG' then
            chkDEBUG.Checked := True
          else if LevelStr = 'WARNING' then
            chkWARN.Checked := True
          else if LevelStr = 'ERROR' then
            chkERROR.Checked := True
          else if LevelStr = 'FATAL' then
            chkFATAL.Checked := True
          else if LevelStr = 'DEVELOP' then
            chkDEVEL.Checked := True;
        end;
      finally
        LevelsList.Free;
      end;
      
      DateFrom.Date := FSearchHistory[SelectedIdx].DateFrom;
      DateTo.Date := FSearchHistory[SelectedIdx].DateTo;

      // 필터 적용
      RefreshData;
    end;
  finally
    Items.Free;
  end;
end;

procedure TfrmLogViewer.btnExportJsonClick(Sender: TObject);
begin
  SaveDialog.Title := 'JSON으로 로그 내보내기';
  SaveDialog.DefaultExt := 'json';
  SaveDialog.Filter := 'JSON 파일 (*.json)|*.json|모든 파일 (*.*)|*.*';
  SaveDialog.InitialDir := ExtractFilePath(ParamStr(0));
  SaveDialog.FileName := 'logs_' + FormatDateTime('yyyymmdd', Date) + '.json';

  if SaveDialog.Execute then
    ExportToJSON(SaveDialog.FileName);
end;

procedure TfrmLogViewer.btnImportJsonClick(Sender: TObject);
begin
  OpenDialog.Title := 'JSON에서 로그 가져오기';
  OpenDialog.DefaultExt := 'json';
  OpenDialog.Filter := 'JSON 파일 (*.json)|*.json|모든 파일 (*.*)|*.*';
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));

  if OpenDialog.Execute then
    ImportFromJSON(OpenDialog.FileName);
end;

procedure TfrmLogViewer.chkMultiLevelClick(Sender: TObject);
begin
  // 다중 레벨 선택 모드 전환
  cbLogLevel.Visible := not chkMultiLevel.Checked;
  clbLogLevels.Visible := chkMultiLevel.Checked;

  // 모드 변경 시 필터 다시 적용
  if not FLoading then
    RefreshData;
end;

procedure TfrmLogViewer.clbLogLevelsClickCheck(Sender: TObject);
begin
  if not FClearingFilter then
    ApplyFilter;
end;

procedure TfrmLogViewer.cbLogLevelChange(Sender: TObject);
begin
  if not FClearingFilter then
    ApplyFilter;
end;

procedure TfrmLogViewer.DateFromChange(Sender: TObject);
begin
  if DateFrom.Date > DateTo.Date then
    DateTo.Date := DateFrom.Date;

  if not FClearingFilter then
    RefreshData;
end;

procedure TfrmLogViewer.DateToChange(Sender: TObject);
begin
  if DateTo.Date < DateFrom.Date then
    DateFrom.Date := DateTo.Date;

  if not FClearingFilter then
    RefreshData;
end;

procedure TfrmLogViewer.edtRefreshIntervalChange(Sender: TObject);
begin
  // 새로고침 간격 변경
  TimerRefresh.Interval := StrToIntDef(edtRefreshInterval.Text, 5) * 1000;
end;

procedure TfrmLogViewer.edtSearchChange(Sender: TObject);
begin
  if not FClearingFilter then
    RefreshData;
end;

procedure TfrmLogViewer.TimerRefreshTimer(Sender: TObject);
begin
  RefreshData;
end;

procedure TfrmLogViewer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  TimerRefresh.Enabled := False;
  LogQuery.Close;
  LogConnection.Disconnect;
end;

procedure TfrmLogViewer.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmLogViewer.miExportCSVClick(Sender: TObject);
begin
  SaveDialog.Title := '로그 내보내기';
  SaveDialog.DefaultExt := 'csv';
  SaveDialog.Filter := 'CSV 파일 (*.csv)|*.csv|모든 파일 (*.*)|*.*';
  SaveDialog.InitialDir := ExtractFilePath(ParamStr(0));
  SaveDialog.FileName := 'logs_' + FormatDateTime('yyyymmdd', Date) + '.csv';

  if SaveDialog.Execute then
    ExportToCSV(SaveDialog.FileName);
end;

procedure TfrmLogViewer.miExportXLSClick(Sender: TObject);
begin
  SaveDialog.Title := '로그 내보내기';
  SaveDialog.DefaultExt := 'xls';
  SaveDialog.Filter := 'XLS 파일 (*.xls)|*.xls|모든 파일 (*.*)|*.*';
  SaveDialog.InitialDir := ExtractFilePath(ParamStr(0));
  SaveDialog.FileName := 'logs_' + FormatDateTime('yyyymmdd', Date) + '.xls';

  if SaveDialog.Execute then
    ExportToXLS(SaveDialog.FileName);
end;

procedure TfrmLogViewer.miExportJSONClick(Sender: TObject);
begin
  btnExportJsonClick(Sender);
end;

procedure TfrmLogViewer.miImportJSONClick(Sender: TObject);
begin
  btnImportJsonClick(Sender);
end;

procedure TfrmLogViewer.miDetailLogClick(Sender: TObject);
begin
  ShowDetailLog;
end;

procedure TfrmLogViewer.miFilteredDeleteClick(Sender: TObject);
begin
  DeleteFilteredLogs;
end;

procedure TfrmLogViewer.ExportToCSV(const FileName: string);
var
  F: TextFile;
  i: Integer;
  Line, CellValue: string;
  CurrentBookmark: TBookmark;
  FileOpened: Boolean;
begin
  if LogQuery.IsEmpty then
  begin
    ShowMessage('내보낼 로그 데이터가 없습니다.');
    Exit;
  end;

  FileOpened := False;
  try
    AssignFile(F, FileName);
    Rewrite(F);
    FileOpened := True;

    // 헤더 쓰기
    Line := '';
    for i := 0 to LogQuery.FieldCount - 1 do
    begin
      if i > 0 then Line := Line + ',';
      Line := Line + '"' + LogQuery.Fields[i].DisplayLabel + '"';
    end;
    WriteLn(F, Line);

    // 현재 위치 저장
    CurrentBookmark := LogQuery.GetBookmark;

    try
      LogQuery.DisableControls;
      LogQuery.First;

      // 데이터 쓰기
      while not LogQuery.EOF do
      begin
        Line := '';
        for i := 0 to LogQuery.FieldCount - 1 do
        begin
          if i > 0 then Line := Line + ',';

          CellValue := LogQuery.Fields[i].AsString;
          // 쉼표나 큰따옴표 처리
          if (Pos(',', CellValue) > 0) or (Pos('"', CellValue) > 0) or (Pos(#13, CellValue) > 0) or (Pos(#10, CellValue) > 0) then
          begin
            CellValue := StringReplace(CellValue, '"', '""', [rfReplaceAll]);
            CellValue := '"' + CellValue + '"';
          end;

          Line := Line + CellValue;
        end;

        WriteLn(F, Line);
        LogQuery.Next;
      end;
    finally
      // 원래 위치로 복원
      if LogQuery.BookmarkValid(CurrentBookmark) then
        LogQuery.GotoBookmark(CurrentBookmark);
      LogQuery.FreeBookmark(CurrentBookmark);
      LogQuery.EnableControls;
    end;

    CloseFile(F);
    FileOpened := False;
    ShowMessage('로그가 성공적으로 내보내졌습니다: ' + FileName);
  except
    on E: Exception do
    begin
      if FileOpened then
      begin
        try
          CloseFile(F);
        except
          // 파일 닫기 중 오류 무시
        end;
      end;
      ShowMessage('로그 내보내기 중 오류가 발생했습니다: ' + E.Message);
    end;
  end;
end;

procedure TfrmLogViewer.ExportToXLS(const FileName: string);
begin
  if LogQuery.IsEmpty then
  begin
    ShowMessage('내보낼 로그 데이터가 없습니다.');
    Exit;
  end;

  RxDBGridExportSpreadSheet.RxDBGrid := RxDBGrid;
  RxDBGridExportSpreadSheet.FileName := FileName;
  RxDBGridExportSpreadSheet.OpenAfterExport := True;
  RxDBGridExportSpreadSheet.Options := [ressExportColors, ressExportFooter,
                                       ressExportFormula, ressExportTitle];
  RxDBGridExportSpreadSheet.ShowSetupForm := True;
  RxDBGridExportSpreadSheet.Execute;
end;

procedure TfrmLogViewer.ExportToJSON(const FileName: string);
begin
  SaveLogsToJSON(FileName);
end;

procedure TfrmLogViewer.ImportFromJSON(const FileName: string);
begin
  LoadLogsFromJSON(FileName);
end;

procedure TfrmLogViewer.SaveLogsToJSON(const FileName: string);
var
  JsonRoot: TMcJsonItem;
  JsonLogs: TMcJsonItem;
  JsonLog: TMcJsonItem;
  LogCount: Integer;
  CurrentBookmark: TBookmark;
begin
  if LogQuery.IsEmpty then
  begin
    ShowMessage('내보낼 로그 데이터가 없습니다.');
    Exit;
  end;

  JsonRoot := TMcJsonItem.Create;
  try
    // 메타 정보 추가
    JsonRoot.Add('export_date').AsString := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);
    JsonRoot.Add('export_type').AsString := 'log_export';
    
    // 로그 배열 생성
    JsonLogs := JsonRoot.Add('logs');
    JsonLogs.ItemType := jitArray;
    
    // 현재 위치 저장
    CurrentBookmark := LogQuery.GetBookmark;
    
    try
      LogQuery.DisableControls;
      LogQuery.First;
      LogCount := 0;
      
      // 로그 데이터 추가
      while not LogQuery.EOF do
      begin
        JsonLog := JsonLogs.Add(IntToStr(LogCount));
        JsonLog.ItemType := jitObject;
        
        // 기본 필드 추가
        JsonLog.Add('id').AsInteger := LogQuery.FieldByName('ID').AsInteger;
        JsonLog.Add('date').AsString := FormatDateTime('yyyy-mm-dd', LogQuery.FieldByName('LDATE').AsDateTime);
        JsonLog.Add('time').AsString := LogQuery.FieldByName('LTIME').AsString;
        JsonLog.Add('level').AsString := LogQuery.FieldByName('LLEVEL').AsString;
        JsonLog.Add('source').AsString := LogQuery.FieldByName('LSOURCE').AsString;
        JsonLog.Add('message').AsString := LogQuery.FieldByName('LMESSAGE').AsString;
        
        // RAW_JSON 필드가 존재하고 비어있지 않으면 추가
        if (LogQuery.FindField('RAW_JSON') <> nil) and 
           (not LogQuery.FieldByName('RAW_JSON').IsNull) and
           (LogQuery.FieldByName('RAW_JSON').AsString <> '') then
        begin
          try
            // JSON 문자열을 파싱하여 JSON 객체로 변환
            JsonLog.Add('raw_data').AsJSON := LogQuery.FieldByName('RAW_JSON').AsString;
          except
            // 파싱 오류 시 그냥 문자열로 저장
            JsonLog.Add('raw_data').AsString := LogQuery.FieldByName('RAW_JSON').AsString;
          end;
        end;
        
        // EXT_DATA_ID 필드가 존재하면 추가
        if (LogQuery.FindField('EXT_DATA_ID') <> nil) and 
           (not LogQuery.FieldByName('EXT_DATA_ID').IsNull) then
        begin
          JsonLog.Add('ext_data_id').AsInteger := LogQuery.FieldByName('EXT_DATA_ID').AsInteger;
        end;
        
        Inc(LogCount);
        LogQuery.Next;
      end;
      
      // 로그 개수 추가
      JsonRoot.Add('log_count').AsInteger := LogCount;
      
      // JSON 파일로 저장
      JsonRoot.SaveToFile(FileName);
      ShowMessage(Format('로그가 성공적으로 내보내졌습니다: %s (%d개 항목)', [FileName, LogCount]));
    finally
      // 원래 위치로 복원
      if LogQuery.BookmarkValid(CurrentBookmark) then
        LogQuery.GotoBookmark(CurrentBookmark);
      LogQuery.FreeBookmark(CurrentBookmark);
      LogQuery.EnableControls;
    end;
  except
    on E: Exception do
      ShowMessage('JSON 내보내기 중 오류가 발생했습니다: ' + E.Message);
  end;
  
  JsonRoot.Free;
end;

procedure TfrmLogViewer.LoadLogsFromJSON(const FileName: string);
var
  JsonRoot: TMcJsonItem;
  JsonLogs: TMcJsonItem;
  JsonLog: TMcJsonItem;
  LogItem: TLogItem;
  i, LogCount: Integer;
  LogDate: TDateTime;
  LogTime: TDateTime;
  RawJson: string;
  ExtDataId: Int64;
begin
  if not FileExists(FileName) then
  begin
    ShowMessage('JSON 파일을 찾을 수 없습니다: ' + FileName);
    Exit;
  end;
  
  if not Assigned(FDBStorage) then
  begin
    ShowMessage('데이터베이스 저장소가 초기화되지 않았습니다.');
    Exit;
  end;
  
  // JSON 파일 로드
  JsonRoot := TMcJsonItem.Create;
  try
    try
      JsonRoot.LoadFromFile(FileName);
      
      // 메타 정보 확인
      if not JsonRoot.HasKey('logs') then
      begin
        ShowMessage('유효하지 않은 로그 JSON 파일입니다.');
        Exit;
      end;
      
      JsonLogs := JsonRoot['logs'];
      LogCount := JsonLogs.Count;
      
      if LogCount = 0 then
      begin
        ShowMessage('JSON 파일에 로그 데이터가 없습니다.');
        Exit;
      end;
      
      // 사용자에게 확인
      if MessageDlg('확인', Format('JSON 파일에서 %d개의 로그를 가져오시겠습니까?', [LogCount]),
         mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
         Exit;
      
      // 로그 항목 가져오기
      for i := 0 to LogCount - 1 do
      begin
        JsonLog := JsonLogs.Items[i];
        
        // 필수 필드 확인
        if not (JsonLog.HasKey('date') and JsonLog.HasKey('time') and
                JsonLog.HasKey('level') and JsonLog.HasKey('message')) then
          Continue;
        
        try
          // 날짜와 시간 변환
          LogDate := StrToDate(JsonLog.S['date']);
          LogTime := StrToTime(JsonLog.S['time']);
          
          // 로그 아이템 생성
          LogItem := TLogItem.Create;
          try
            LogItem.Level := StrToLogLevel(JsonLog.S['level']);
            LogItem.TimeStamp := LogDate + LogTime;
            LogItem.Message := JsonLog.S['message'];
            
            if JsonLog.HasKey('source') then
              LogItem.Source := JsonLog.S['source']
            else
              LogItem.Source := '';
            
            // RAW_JSON 확인
            if JsonLog.HasKey('raw_data') then
            begin
              if JsonLog['raw_data'].ItemType = jitObject then
                RawJson := JsonLog['raw_data'].AsJSON
              else
                RawJson := JsonLog.S['raw_data'];
              
              // EXT_DATA_ID 확인
              if JsonLog.HasKey('ext_data_id') then
                ExtDataId := JsonLog.I['ext_data_id']
              else
                ExtDataId := 0;
              
              // 확장 데이터로 저장
              FDBStorage.DoStoreLogWithExtData(LogItem, RawJson);
            end
            else
            begin
              // 일반 로그로 저장
              FDBStorage.DoStoreLog(LogItem);
            end;
          finally
            LogItem.Free;
          end;
        except
          on E: Exception do
            ; // 개별 로그 항목 처리 중 오류는 무시하고 계속 진행
        end;
      end;
      
      // 데이터 새로고침
      RefreshData;
      ShowMessage(Format('JSON 파일에서 %d개의 로그를 성공적으로 가져왔습니다.', [LogCount]));
    except
      on E: Exception do
        ShowMessage('JSON 가져오기 중 오류가 발생했습니다: ' + E.Message);
    end;
  finally
    JsonRoot.Free;
  end;
end;

procedure TfrmLogViewer.UpdateRowCount;
begin
  // 로그 수 표시
  if LogQuery.IsEmpty then
    lblCount.Caption := '로그 없음'
  else
    lblCount.Caption := Format('로그 수: %d', [LogQuery.RecordCount]);
end;

procedure TfrmLogViewer.miFindDlgClick(Sender: TObject);
begin
  RxDBGrid.ShowFindDialog;
end;

procedure TfrmLogViewer.miColumnsDlgClick(Sender: TObject);
begin
  RxDBGrid.ShowColumnsDialog;
end;

procedure TfrmLogViewer.miFilterDlgClick(Sender: TObject);
begin
  RxDBGrid.ShowFilterDialog;
end;

procedure TfrmLogViewer.miSortDlgClick(Sender: TObject);
begin
  RxDBGrid.ShowSortDialog;
end;

procedure TfrmLogViewer.miOptimizeColumnsClick(Sender: TObject);
begin
  RxDBGrid.OptimizeColumnsWidthAll;
end;

procedure TfrmLogViewer.ShowDetailLog;
var
  DetailForm: TForm;
  Memo: TMemo;
  Level, Source, TimeStamp, Msg: string;
  RawJson: string;
  JsonObj: TMcJsonItem;
  HasExtData: Boolean;
begin
  if LogQuery.IsEmpty then Exit;

  Level := LogQuery.FieldByName('LLEVEL').AsString;
  Source := LogQuery.FieldByName('LSOURCE').AsString;
  TimeStamp := FormatDateTime('yyyy-mm-dd ', LogQuery.FieldByName('LDATE').AsDateTime) +
               LogQuery.FieldByName('LTIME').AsString;
  Msg := LogQuery.FieldByName('LMESSAGE').AsString;
  
  // RAW_JSON 필드가 있고 비어있지 않은지 확인
  HasExtData := (LogQuery.FindField('RAW_JSON') <> nil) and 
                (not LogQuery.FieldByName('RAW_JSON').IsNull) and
                (LogQuery.FieldByName('RAW_JSON').AsString <> '');
  
  if HasExtData then
    RawJson := LogQuery.FieldByName('RAW_JSON').AsString
  else
    RawJson := '';

  DetailForm := TForm.Create(Self);
  try
    DetailForm.Caption := Format('로그 상세 정보 - %s [%s] %s', [TimeStamp, Level, Source]);
    DetailForm.Width := 600;
    DetailForm.Height := 400;
    DetailForm.Position := poScreenCenter;

    Memo := TMemo.Create(DetailForm);
    Memo.Parent := DetailForm;
    Memo.Align := alClient;
    Memo.ScrollBars := ssVertical;
    Memo.ReadOnly := True;
    Memo.Font.Name := 'Consolas';
    Memo.Font.Size := 10;

    Memo.Lines.Add('시간: ' + TimeStamp);
    Memo.Lines.Add('레벨: ' + Level);
    Memo.Lines.Add('소스: ' + Source);
    Memo.Lines.Add('');
    Memo.Lines.Add('메시지:');
    Memo.Lines.Add('--------');
    Memo.Lines.Add(Msg);
    
    // RAW_JSON 데이터가 있으면 추가
    if HasExtData then
    begin
      Memo.Lines.Add('');
      Memo.Lines.Add('확장 데이터:');
      Memo.Lines.Add('--------');
      
      try
        // JSON 형식으로 파싱을 시도하여 보기 좋게 출력
        JsonObj := TMcJsonItem.Create(RawJson);
        try
          Memo.Lines.Add(JsonObj.FormatJSON);
        finally
          JsonObj.Free;
        end;
      except
        // 파싱 실패 시 원본 문자열 출력
        Memo.Lines.Add(RawJson);
      end;
    end;

    DetailForm.ShowModal;
  finally
    DetailForm.Free;
  end;
end;

procedure TfrmLogViewer.DeleteFilteredLogs;
var
  SelectedLevels: TStringList;
  SearchStr: string;
  i: Integer;
  DeleteSQL: string;
  DeleteQuery: TZQuery;
  LogTables: TStringList;
  StartYearMonth, EndYearMonth: string;
  TableCount, DeletedCount: Integer;
begin
  if LogQuery.IsEmpty then
  begin
    ShowMessage('삭제할 로그가 없습니다.');
    Exit;
  end;

  if MessageDlg('경고', '현재 필터링된 로그 ' + IntToStr(LogQuery.RecordCount) +
                '개를 모두 삭제하시겠습니까? 이 작업은 되돌릴 수 없습니다.',
     mtWarning, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  // 로그 레벨 목록
  SelectedLevels := GetSelectedLevels;
  try
    // 검색어
    SearchStr := Trim(edtSearch.Text);

    if Assigned(FDBStorage) then
    begin
      // 월별 테이블 방식
      TableCount := 0;
      DeletedCount := 0;
      
      // 년월 범위 문자열 계산
      StartYearMonth := FormatDateTime('YYYYMM', DateFrom.Date);
      EndYearMonth := FormatDateTime('YYYYMM', DateTo.Date);
      
      // FDBStorage로부터 년월 범위에 해당하는 테이블 목록 가져오기
      LogTables := FDBStorage.GetTablesBetweenDates(DateFrom.Date, DateTo.Date);
      try
        // 각 테이블별로 삭제 실행
        for i := 0 to LogTables.Count - 1 do
        begin
          DeleteSQL := 'DELETE FROM ' + LogTables[i] +
                      ' WHERE LDATE >= :StartDate AND LDATE <= :EndDate';

          // 로그 레벨 필터 추가
          if (SelectedLevels.Count > 0) and (SelectedLevels.Count < 6) then
          begin
            DeleteSQL := DeleteSQL + ' AND (';
            for i := 0 to SelectedLevels.Count - 1 do
            begin
              if i > 0 then DeleteSQL := DeleteSQL + ' OR ';
              DeleteSQL := DeleteSQL + 'LLEVEL = ''' + SelectedLevels[i] + '''';
            end;
            DeleteSQL := DeleteSQL + ')';
          end;

          // 검색어 필터 추가
          if SearchStr <> '' then
          begin
            DeleteSQL := DeleteSQL + Format(' AND ((LSOURCE LIKE ''%%%s%%'') OR (LMESSAGE LIKE ''%%%s%%''))',
                        [SearchStr, SearchStr]);
          end;

          // 삭제 쿼리 실행
          DeleteQuery := TZQuery.Create(nil);
          try
            DeleteQuery.Connection := LogConnection;
            DeleteQuery.SQL.Text := DeleteSQL;
            DeleteQuery.ParamByName('StartDate').AsDate := DateFrom.Date;
            DeleteQuery.ParamByName('EndDate').AsDate := DateTo.Date;
            DeletedCount := DeletedCount + DeleteQuery.ExecSQL;
            Inc(TableCount);
          finally
            DeleteQuery.Free;
          end;
        end;
      finally
        LogTables.Free;
      end;
      
      // 성공 메시지
      if TableCount > 0 then
        ShowMessage(Format('총 %d개의 테이블에서 %d개의 로그가 삭제되었습니다.', 
                           [TableCount, DeletedCount]))
      else
        ShowMessage('삭제할 로그가 없습니다.');
    end
    else
    begin
      // 단일 테이블 방식 (이전 버전 호환)
      DeleteSQL := 'DELETE FROM LOGS WHERE LDATE >= :StartDate AND LDATE <= :EndDate';

      // 로그 레벨 필터 추가
      if (SelectedLevels.Count > 0) and (SelectedLevels.Count < 6) then
      begin
        DeleteSQL := DeleteSQL + ' AND (';
        for i := 0 to SelectedLevels.Count - 1 do
        begin
          if i > 0 then DeleteSQL := DeleteSQL + ' OR ';
          DeleteSQL := DeleteSQL + 'LLEVEL = ''' + SelectedLevels[i] + '''';
        end;
        DeleteSQL := DeleteSQL + ')';
      end;

      // 검색어 필터 추가
      if SearchStr <> '' then
      begin
        DeleteSQL := DeleteSQL + Format(' AND ((LSOURCE LIKE ''%%%s%%'') OR (LMESSAGE LIKE ''%%%s%%''))',
                    [SearchStr, SearchStr]);
      end;

      // 삭제 쿼리 실행
      LogQuery.Close;
      LogQuery.SQL.Text := DeleteSQL;
      LogQuery.ParamByName('StartDate').AsDate := DateFrom.Date;
      LogQuery.ParamByName('EndDate').AsDate := DateTo.Date;
      
      // 삭제된 행 수 가져오기
      DeletedCount := LogQuery.ExecSQL;
      ShowMessage(Format('%d개의 로그가 삭제되었습니다.', [DeletedCount]));
    end;
  finally
    SelectedLevels.Free;
  end;

  // 데이터 새로고침
  RefreshData;
end;

procedure TfrmLogViewer.LoadSearchHistory;
var
  IniFile: TIniFile;
  i, Count: Integer;
  Section: string;
begin
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'LogViewer.ini');
  try
    Count := IniFile.ReadInteger('SearchHistory', 'Count', 0);
    SetLength(FSearchHistory, Count);

    for i := 0 to Count - 1 do
    begin
      Section := 'Search_' + IntToStr(i);
      FSearchHistory[i].SearchText := IniFile.ReadString(Section, 'SearchText', '');
      FSearchHistory[i].LogLevel := IniFile.ReadString(Section, 'LogLevel', '');
      FSearchHistory[i].DateFrom := StrToDateDef(IniFile.ReadString(Section, 'DateFrom', ''), Date - 7);
      FSearchHistory[i].DateTo := StrToDateDef(IniFile.ReadString(Section, 'DateTo', ''), Date);
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TfrmLogViewer.SaveSearchHistory;
var
  IniFile: TIniFile;
  i: Integer;
  Section: string;
begin
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'LogViewer.ini');
  try
    IniFile.WriteInteger('SearchHistory', 'Count', Length(FSearchHistory));

    for i := 0 to High(FSearchHistory) do
    begin
      Section := 'Search_' + IntToStr(i);
      IniFile.WriteString(Section, 'SearchText', FSearchHistory[i].SearchText);
      IniFile.WriteString(Section, 'LogLevel', FSearchHistory[i].LogLevel);
      IniFile.WriteString(Section, 'DateFrom', FormatDateTime('yyyy-mm-dd', FSearchHistory[i].DateFrom));
      IniFile.WriteString(Section, 'DateTo', FormatDateTime('yyyy-mm-dd', FSearchHistory[i].DateTo));
    end;
  finally
    IniFile.Free;
  end;
end;

function SelectFromList(const Title, Prompt: string; Items: TStrings): Integer;
var
  Form: TForm;
  Label1: TLabel;
  ListBox: TListBox;
  ButtonPanel: TPanel;
  OKButton, CancelButton: TButton;
begin
  Result := -1;

  Form := TForm.Create(nil);
  try
    Form.Caption := Title;
    Form.Position := poScreenCenter;
    Form.BorderStyle := bsDialog;
    Form.Width := 350;
    Form.Height := 300;

    Label1 := TLabel.Create(Form);
    Label1.Parent := Form;
    Label1.Caption := Prompt;
    Label1.Left := 8;
    Label1.Top := 8;

    ListBox := TListBox.Create(Form);
    ListBox.Parent := Form;
    ListBox.Left := 8;
    ListBox.Top := 24;
    ListBox.Width := Form.ClientWidth - 16;
    ListBox.Height := Form.ClientHeight - 80;
    ListBox.Items.Assign(Items);
    if ListBox.Items.Count > 0 then
      ListBox.ItemIndex := 0;

    ButtonPanel := TPanel.Create(Form);
    ButtonPanel.Parent := Form;
    ButtonPanel.Align := alBottom;
    ButtonPanel.Height := 40;
    ButtonPanel.BevelOuter := bvNone;

    OKButton := TButton.Create(Form);
    OKButton.Parent := ButtonPanel;
    OKButton.Caption := '확인';
    OKButton.ModalResult := mrOK;
    OKButton.Left := ButtonPanel.Width - 160;
    OKButton.Top := 8;
    OKButton.Width := 75;

    CancelButton := TButton.Create(Form);
    CancelButton.Parent := ButtonPanel;
    CancelButton.Caption := '취소';
    CancelButton.ModalResult := mrCancel;
    CancelButton.Left := ButtonPanel.Width - 80;
    CancelButton.Top := 8;
    CancelButton.Width := 75;

    if Form.ShowModal = mrOK then
      Result := ListBox.ItemIndex;
  finally
    Form.Free;
  end;
end;

end.
