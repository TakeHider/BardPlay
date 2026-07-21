using BardPlay2.Core.Config;
using BardPlay2.Core.Keyboard;
using BardPlay2.Core.Logging;

namespace BardPlay2.Core.Engine;

/// <summary>
/// FF14Engine / BlueProtocolEngine 共通の下ごしらえをまとめた基底クラス。
/// ゲーム固有の演奏ルール(モノ/ポリフォニック)は派生クラスで実装する。
/// </summary>
public abstract class GameEngineBase : IGameEngine
{
    protected readonly GameProfile Profile;
    protected readonly IKeyboardSender Sender;
    protected readonly PressedKeyManager PressedKeys;
    protected readonly DebugLogger? Logger;

    /// <summary>トランスポーズ(半音単位、オクターブなら±12刻みで呼び出し側が計算)</summary>
    public int Transpose { get; set; }

    public string Name => Profile.Name;

    protected GameEngineBase(GameProfile profile, IKeyboardSender sender, DebugLogger? logger = null)
    {
        Profile = profile;
        Sender = sender;
        Logger = logger;
        PressedKeys = new PressedKeyManager();
    }

    public abstract void NoteOn(int note, int velocity);
    public abstract void NoteOff(int note);

    public virtual void ReleaseAll() => PressedKeys.ReleaseAll(Sender);

    /// <summary>
    /// トランスポーズを適用したノート番号に対応するキー割り当てを取得する。
    /// マッピングが無い(=演奏範囲外)場合は false を返す。
    /// </summary>
    protected bool TryResolveKeyStroke(int note, out int mappedNote, out KeyStroke keyStroke)
    {
        mappedNote = note + Transpose;
        keyStroke = default;

        if (mappedNote < 0 || mappedNote > 127) return false;
        if (!Profile.KeyMapping.TryGetValue(mappedNote, out var binding)) return false;
        if (string.IsNullOrWhiteSpace(binding.Key)) return false;
        if (!VirtualKeyTable.TryGetVirtualKey(binding.Key, out var vk)) return false;

        keyStroke = new KeyStroke(vk, binding.Shift, binding.Ctrl, binding.Alt);
        return true;
    }

    protected static string NoteName(int note)
    {
        // 中央のCをC4とする一般的な表記(MIDIノート60 = C4)
        string[] names = { "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" };
        int octave = (note / 12) - 1;
        return $"{names[note % 12]}{octave}";
    }
}
