using BardPlay2.Core.Config;
using BardPlay2.Core.Engine;
using BardPlay2.Core.Keyboard;
using BardPlay2.Core.Logging;
using BardPlay2.Core.Midi;

namespace BardPlay2.UI;

public partial class MainForm : Form
{
    private readonly ConfigManager _configManager = new();
    private readonly MidiInputService _midiInput = new();
    private readonly DebugLogger _logger = new();

    private AppConfig _config = new();
    private List<GameProfile> _profiles = new();
    private GameProfile? _selectedProfile;
    private IGameEngine? _engine;
    private IKeyboardSender? _sender;

    private readonly Dictionary<string, RadioButton> _gameRadios = new();
    private readonly Dictionary<string, RadioButton> _senderRadios = new();

    private bool _isRunning;
    private bool _suppressEvents; // ラジオボタン等をコードから変更する時にイベント再入を防ぐ

    public MainForm()
    {
        InitializeComponent();
        _logger.Logged += OnLogged;
    }

    private void MainForm_Load(object? sender, EventArgs e)
    {
        _config = _configManager.LoadConfig();
        _profiles = _configManager.LoadProfiles();

        ApplyBackgroundColor(_config.BackgroundColor);

        BuildGameRadioButtons();
        BuildSenderRadioButtons();

        _suppressEvents = true;
        // 注意: 疑似和音(VirtualChords)は設定として保存されるが、
        // NAudioはノートを1件ずつイベント配信するため、Delphi版のようなバッファ単位の
        // 昇順ソートは未実装。同時押しの順序制御が必要な場合はMidiInputService側で
        // 一定時間分のノートをバッファしてから昇順ソート・ディスパッチする拡張を追加する。
        chkVirtualChords.Checked = _config.VirtualChords;
        chkStartOnRun.Checked = _config.StartOnRun;
        chkDebug.Checked = _config.DebugMode;
        cboOctave.SelectedIndex = Math.Clamp(_config.Octave + 2, 0, 4);
        _suppressEvents = false;

        RefreshDeviceList();
        SelectGame(_config.Game);
        SelectSender(_config.KeyboardSender);

        _logger.IsEnabled = _config.DebugMode;

        if (_config.StartOnRun && cboDevice.Items.Count > 0)
        {
            StartPlaying();
        }
    }

    private void MainForm_FormClosing(object? sender, FormClosingEventArgs e)
    {
        if (_isRunning)
        {
            StopPlaying();
        }

        SaveCurrentSettingsToConfig();
        _configManager.SaveConfig(_config);
        _midiInput.Dispose();
    }

    // ------------------------------------------------------------------
    // MIDIデバイス一覧
    // ------------------------------------------------------------------

    private void btnRefresh_Click(object? sender, EventArgs e) => RefreshDeviceList();

    private void RefreshDeviceList()
    {
        var previouslySelected = cboDevice.SelectedItem as string ?? _config.MidiDevice;

        cboDevice.Items.Clear();
        var names = MidiInputService.GetDeviceNames();
        foreach (var name in names)
        {
            cboDevice.Items.Add(name);
        }

        if (cboDevice.Items.Count > 0)
        {
            var index = cboDevice.Items.IndexOf(previouslySelected);
            cboDevice.SelectedIndex = index >= 0 ? index : 0;
            btnStart.Enabled = true;
        }
        else
        {
            btnStart.Enabled = false;
            lblStatus.Text = "*** MIDI Device Not Found ***";
        }
    }

    // ------------------------------------------------------------------
    // ゲーム選択 / 送信方式選択
    // ------------------------------------------------------------------

    private void BuildGameRadioButtons()
    {
        flowGame.Controls.Clear();
        _gameRadios.Clear();

        foreach (var profile in _profiles)
        {
            var radio = new RadioButton
            {
                Text = profile.Name,
                AutoSize = true,
                Tag = profile.Name
            };
            radio.CheckedChanged += GameRadio_CheckedChanged;
            flowGame.Controls.Add(radio);
            _gameRadios[profile.Name] = radio;
        }
    }

    private void BuildSenderRadioButtons()
    {
        flowSender.Controls.Clear();
        _senderRadios.Clear();

        foreach (var senderName in KeyboardSenderFactory.AvailableSenderNames)
        {
            var radio = new RadioButton
            {
                Text = senderName,
                AutoSize = true,
                Tag = senderName
            };
            radio.CheckedChanged += SenderRadio_CheckedChanged;
            flowSender.Controls.Add(radio);
            _senderRadios[senderName] = radio;
        }
    }

    private void SelectGame(string gameName)
    {
        if (!_gameRadios.TryGetValue(gameName, out var radio))
        {
            radio = _gameRadios.Values.FirstOrDefault();
        }

        if (radio == null) return;

        _suppressEvents = true;
        radio.Checked = true;
        _suppressEvents = false;

        ApplySelectedGame((string)radio.Tag!);
    }

    private void SelectSender(string senderName)
    {
        if (!_senderRadios.TryGetValue(senderName, out var radio))
        {
            radio = _senderRadios.Values.FirstOrDefault();
        }

        if (radio == null) return;

        _suppressEvents = true;
        radio.Checked = true;
        _suppressEvents = false;

        _config.KeyboardSender = (string)radio.Tag!;
    }

    private void GameRadio_CheckedChanged(object? sender, EventArgs e)
    {
        if (_suppressEvents) return;
        if (sender is not RadioButton { Checked: true } radio) return;

        ApplySelectedGame((string)radio.Tag!);
    }

    private void SenderRadio_CheckedChanged(object? sender, EventArgs e)
    {
        if (_suppressEvents) return;
        if (sender is not RadioButton { Checked: true } radio) return;

        _config.KeyboardSender = (string)radio.Tag!;
    }

    private void ApplySelectedGame(string gameName)
    {
        _selectedProfile = _profiles.FirstOrDefault(p => p.Name == gameName);
        _config.Game = gameName;

        // ゲームごとの推奨送信方式に切り替える(ユーザーが後で変更可能)
        if (_selectedProfile != null && _senderRadios.ContainsKey(_selectedProfile.DefaultKeyboardSender))
        {
            SelectSender(_selectedProfile.DefaultKeyboardSender);
        }
    }

    // ------------------------------------------------------------------
    // 開始 / 停止
    // ------------------------------------------------------------------

    private void btnStart_Click(object? sender, EventArgs e)
    {
        if (_isRunning)
            StopPlaying();
        else
            StartPlaying();
    }

    private void StartPlaying()
    {
        if (_selectedProfile == null || string.IsNullOrWhiteSpace(_selectedProfile.EngineId))
        {
            MessageBox.Show(this, "有効なゲームプロファイルが選択されていません。プロファイルを確認してください。", "BardPlay2",
                MessageBoxButtons.OK, MessageBoxIcon.Warning);
            return;
        }

        if (cboDevice.SelectedIndex < 0)
        {
            MessageBox.Show(this, "MIDIデバイスが選択されていません。", "BardPlay2",
                MessageBoxButtons.OK, MessageBoxIcon.Warning);
            return;
        }

        SaveCurrentSettingsToConfig();

        _sender = KeyboardSenderFactory.Create(_config.KeyboardSender, _logger);
        _sender.BeginSession();

        _engine = GameEngineFactory.Create(_selectedProfile, _sender, _logger);
        if (_engine is GameEngineBase baseEngine)
        {
            baseEngine.Transpose = _config.Octave * 12;
        }

        _midiInput.NoteOn += MidiInput_NoteOn;
        _midiInput.NoteOff += MidiInput_NoteOff;
        _midiInput.DeviceError += MidiInput_DeviceError;
        _midiInput.RawMessageReceived += MidiInput_RawMessageReceived;
        _midiInput.Open(cboDevice.SelectedIndex);

        _isRunning = true;
        btnStart.Text = "Stop";
        cboDevice.Enabled = false;
        btnRefresh.Enabled = false;
        cboOctave.Enabled = false;
        foreach (var radio in _gameRadios.Values) radio.Enabled = false;
        lblStatus.Text = $"演奏中 - {_selectedProfile.Name} / {_config.KeyboardSender}";
    }

    private void StopPlaying()
    {
        _midiInput.NoteOn -= MidiInput_NoteOn;
        _midiInput.NoteOff -= MidiInput_NoteOff;
        _midiInput.DeviceError -= MidiInput_DeviceError;
        _midiInput.RawMessageReceived -= MidiInput_RawMessageReceived;
        _midiInput.Close();

        _engine?.ReleaseAll();
        _engine = null;
        _sender = null;

        _isRunning = false;
        btnStart.Text = "Start";
        cboDevice.Enabled = true;
        btnRefresh.Enabled = true;
        cboOctave.Enabled = true;
        foreach (var radio in _gameRadios.Values) radio.Enabled = true;
        lblStatus.Text = "停止中";
    }

    // ------------------------------------------------------------------
    // MIDIイベント (バックグラウンドスレッドから呼ばれる)
    // ------------------------------------------------------------------

    private void MidiInput_NoteOn(object? sender, MidiNoteEventArgs e) => _engine?.NoteOn(e.Note, e.Velocity);

    private void MidiInput_NoteOff(object? sender, MidiNoteEventArgs e) => _engine?.NoteOff(e.Note);

    private void MidiInput_RawMessageReceived(object? sender, string message) => _logger.Log(message);

    private void MidiInput_DeviceError(object? sender, string message)
    {
        if (InvokeRequired)
        {
            BeginInvoke(() => MidiInput_DeviceError(sender, message));
            return;
        }

        lblStatus.Text = message;
        if (_isRunning) StopPlaying();
    }

    // ------------------------------------------------------------------
    // その他コントロール
    // ------------------------------------------------------------------

    private void cboOctave_SelectedIndexChanged(object? sender, EventArgs e)
    {
        if (_suppressEvents) return;
        _config.Octave = cboOctave.SelectedIndex - 2;
        if (_engine is GameEngineBase baseEngine)
        {
            baseEngine.Transpose = _config.Octave * 12;
        }
    }

    private void chkDebug_CheckedChanged(object? sender, EventArgs e)
    {
        _logger.IsEnabled = chkDebug.Checked;
        _config.DebugMode = chkDebug.Checked;
    }

    private void OnLogged(object? sender, LogEntry entry)
    {
        if (InvokeRequired)
        {
            BeginInvoke(() => OnLogged(sender, entry));
            return;
        }

        txtLog.AppendText(entry + Environment.NewLine);
    }

    private void ApplyBackgroundColor(string hexColor)
    {
        try
        {
            if (hexColor.Length == 7 && hexColor[0] == '#')
            {
                var r = Convert.ToInt32(hexColor.Substring(1, 2), 16);
                var g = Convert.ToInt32(hexColor.Substring(3, 2), 16);
                var b = Convert.ToInt32(hexColor.Substring(5, 2), 16);
                BackColor = Color.FromArgb(r, g, b);
            }
        }
        catch (Exception)
        {
            // 不正な色指定は無視して既定色のまま
        }
    }

    private void SaveCurrentSettingsToConfig()
    {
        _config.MidiDevice = cboDevice.SelectedItem as string ?? _config.MidiDevice;
        _config.VirtualChords = chkVirtualChords.Checked;
        _config.StartOnRun = chkStartOnRun.Checked;
        _config.DebugMode = chkDebug.Checked;
        _config.Octave = cboOctave.SelectedIndex - 2;
    }
}
