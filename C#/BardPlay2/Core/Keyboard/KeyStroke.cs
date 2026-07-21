namespace BardPlay2.Core.Keyboard;

/// <summary>
/// ゲームエンジンからKeyboardSenderへ渡す、1回のキー操作を表す値。
/// 仕様書 7章「KeyStroke」に対応。
/// </summary>
public readonly struct KeyStroke : IEquatable<KeyStroke>
{
    /// <summary>送信する仮想キーコード(VK_*)</summary>
    public byte VirtualKey { get; }

    public bool Shift { get; }
    public bool Ctrl { get; }
    public bool Alt { get; }

    public KeyStroke(byte virtualKey, bool shift = false, bool ctrl = false, bool alt = false)
    {
        VirtualKey = virtualKey;
        Shift = shift;
        Ctrl = ctrl;
        Alt = alt;
    }

    public bool Equals(KeyStroke other) =>
        VirtualKey == other.VirtualKey && Shift == other.Shift && Ctrl == other.Ctrl && Alt == other.Alt;

    public override bool Equals(object? obj) => obj is KeyStroke other && Equals(other);

    public override int GetHashCode() => HashCode.Combine(VirtualKey, Shift, Ctrl, Alt);

    public override string ToString()
    {
        var mods = string.Concat(
            Ctrl ? "Ctrl+" : "",
            Alt ? "Alt+" : "",
            Shift ? "Shift+" : "");
        return $"{mods}0x{VirtualKey:X2}";
    }
}
