namespace BardPlay2.Core.Config;

/// <summary>
/// アプリ全体の設定。JSON形式で保存する。
/// 仕様書 10章「設定保存」に対応。
/// </summary>
public sealed class AppConfig
{
    /// <summary>最後に使用したMIDIデバイス名</summary>
    public string MidiDevice { get; set; } = "";

    /// <summary>選択中のゲーム(プロファイル名)</summary>
    public string Game { get; set; } = "FF14";

    /// <summary>キーボード送信方式("SendInput" / "SendMessage" / "PostMessage")</summary>
    public string KeyboardSender { get; set; } = "SendInput";

    /// <summary>オクターブ補正 (-2 ～ +2)</summary>
    public int Octave { get; set; } = 0;

    /// <summary>起動時に自動でMIDI受信を開始するか</summary>
    public bool StartOnRun { get; set; }

    /// <summary>疑似和音(同時に来た複数ノートを昇順に処理する)を有効にするか</summary>
    public bool VirtualChords { get; set; } = true;

    /// <summary>デバッグ表示を有効にするか</summary>
    public bool DebugMode { get; set; }

    /// <summary>デバッグログをファイルにも保存するか</summary>
    public bool DebugSaveToFile { get; set; }

    /// <summary>範囲外の音が出たら演奏を止めるか</summary>
    public bool StopOnOutOfRange { get; set; }

    /// <summary>メイン画面の背景色(#RRGGBB)</summary>
    public string BackgroundColor { get; set; } = "#F5FFFA";
}
