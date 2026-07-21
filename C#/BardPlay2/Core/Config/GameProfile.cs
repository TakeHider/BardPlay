namespace BardPlay2.Core.Config;

/// <summary>演奏方式。仕様書 4章「演奏方式」に対応。</summary>
public enum PlayMode
{
    Monophonic,
    Polyphonic
}

/// <summary>
/// 1つのノートに割り当てるキーとその修飾キー。
/// KeyStroke(仕様書7章)と1:1で対応するJSON表現。
/// 例: { "key": "a", "shift": true }  =>  Shift + A
/// </summary>
public sealed class KeyBinding
{
    public string Key { get; set; } = "";
    public bool Shift { get; set; }
    public bool Ctrl { get; set; }
    public bool Alt { get; set; }
}

/// <summary>
/// ゲームごとの演奏設定をまとめたプロファイル。
/// 仕様書 4章「ゲームプロファイル」に対応。
/// キー配置・演奏方式・修飾キー・既定のキー送信方式をすべてここに持たせることで、
/// ゲーム固有の違いをIGameEngine実装とこのプロファイルだけに閉じ込める。
/// </summary>
public sealed class GameProfile
{
    /// <summary>プロファイル名 (GUI表示名にもなる。例: "FF14")</summary>
    public string Name { get; set; } = "";

    /// <summary>このゲームを担当する IGameEngine の識別子 (例: "FF14", "BlueProtocol")</summary>
    public string EngineId { get; set; } = "";

    /// <summary>演奏方式(モノフォニック/ポリフォニック)</summary>
    public PlayMode PlayMode { get; set; } = PlayMode.Monophonic;

    /// <summary>このゲームの既定のキー送信方式</summary>
    public string DefaultKeyboardSender { get; set; } = "SendInput";

    /// <summary>ノート番号(0-127、トランスポーズ適用後)からキー割り当てへのマッピング</summary>
    public Dictionary<int, KeyBinding> KeyMapping { get; set; } = new();
}
